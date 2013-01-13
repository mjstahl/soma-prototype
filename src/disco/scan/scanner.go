// Copyright (C) 2013 Mark Stahl

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.

// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package scan

import (
	"disco/file"
	"fmt"
	"path/filepath"
	"unicode"
	"unicode/utf8"
)

type ErrorHandler func(pos file.Position, msg string)

type Scanner struct {
	// immutable state
	file *file.File   // source file handle
	dir  string       // directory portion of file.Name()
	src  []byte       // source
	err  ErrorHandler // error reporting; or nil

	// scanning state
	ch         rune // current character
	offset     int  // character offset
	rdOffset   int  // reading offset (position after current character)
	lineOffset int  // current line offset
}

func (s *Scanner) Init(file *file.File, src []byte, err ErrorHandler) {
	if file.Size() != len(src) {
		panic(fmt.Sprintf("file size (%d) does not match src len (%d)", file.Size(), len(src)))
	}

	s.file = file
	s.dir, _ = filepath.Split(file.Name())
	s.src = src
	s.err = err

	s.ch = ' '
	s.offset = 0
	s.rdOffset = 0
	s.lineOffset = 0

	s.next()
}

func (s *Scanner) Scan() (pos file.Pos, tok Token, lit string) {
	s.skipWhitespace()

	// current token start
	pos = s.file.Pos(s.offset)

	// determine token value
	switch ch := s.ch; {
	case isUpper(ch):
		lit = s.scanIdentifier()
		tok = NAME
	case isLower(ch):
		lit = s.scanIdentifier()
		if s.ch == ':' {
			lit = lit + ":"
			s.next()
			tok = KEYWORD
		} else {
			tok = IDENT
		}
	default:
		s.next()
		switch ch {
		case -1:
			tok = EOF
		case '+':
			tok = PLUS
		case '=':
			if s.ch == '>' {
				s.next()
				tok = DEFINE
			}
		case '{':
			tok = LBRACK
		case '}':
			tok = RBRACK
		default:
			s.error(s.file.Offset(pos), fmt.Sprintf("illegal character %#U", ch))
			tok = ILLEGAL
			lit = string(ch)
		}
	}

	return
}

func (s *Scanner) next() {
	if s.rdOffset < len(s.src) {
		s.offset = s.rdOffset
		if s.ch == '\n' {
			s.lineOffset = s.offset
			s.file.AddLine(s.offset)
		}

		r, w := rune(s.src[s.rdOffset]), 1
		switch {
		case r == 0:
			s.error(s.offset, "illegal character NULL")
		case r >= 0x80:
			// not ASCII
			r, w = utf8.DecodeRune(s.src[s.rdOffset:])
			if r == utf8.RuneError && w == 1 {
				s.error(s.offset, "illegal UTF-8 encoding")
			}
		}
		s.rdOffset += w
		s.ch = r
	} else {
		s.offset = len(s.src)
		if s.ch == '\n' {
			s.lineOffset = s.offset
			s.file.AddLine(s.offset)
		}
		s.ch = -1 // eof
	}
}

func (s *Scanner) error(offset int, msg string) {
	if s.err != nil {
		s.err(s.file.Position(s.file.Pos(offset)), msg)
	}
}

func (s *Scanner) skipWhitespace() {
	for s.ch == ' ' || s.ch == '\t' || s.ch == '\n' || s.ch == '\r' {
		s.next()
	}
}

func (s *Scanner) scanIdentifier() string {
	offs := s.offset
	for isLetter(s.ch) {
		s.next()
	}

	return string(s.src[offs:s.offset])
}

func isLetter(ch rune) bool {
	return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch >= 0x80 && unicode.IsLetter(ch)
}

func isUpper(ch rune) bool {
	return 'A' <= ch && ch <= 'Z' || ch >= 0x80 && unicode.IsLetter(ch)
}

func isLower(ch rune) bool {
	return 'a' <= ch && ch <= 'z' || ch >= 0x80 && unicode.IsLetter(ch)
}
