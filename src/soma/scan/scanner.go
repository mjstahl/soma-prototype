package scan

import (
	"fmt"
	"path/filepath"
	"soma/file"
	"unicode"
	"unicode/utf8"
)

type ErrorHandler func(pos file.Position, msg string)

type Scanner struct {
	file *file.File   // source file handle
	dir  string       // directory portion of file.Name()
	src  []byte       // source
	err  ErrorHandler // error reporting; or nil

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

	pos = s.file.Pos(s.offset)

	switch ch := s.ch; {
	case ch == '-':
		s.next()
		if digitVal(s.ch) < 10 {
			tok, lit = s.scanNumber()
			lit = "-" + lit
		} else {
			tok, lit = BINARY, s.scanBinary()
			lit = "-" + lit
		}
	case isUpper(ch):
		tok, lit = GLOBAL, s.scanIdentifier()
	case isLower(ch) || ch == '@':
		lit = s.scanIdentifier()
		switch {
		case s.ch == ':':
			lit = lit + ":"
			s.next()
			tok = KEYWORD
		default:
			tok = IDENT
		}
	case isBinary(ch):
		bin := s.scanBinary()
		if bin == "=>" {
			tok, lit = DEFINE, "=>"
		} else {
			tok, lit = BINARY, bin
		}
	case digitVal(ch) < 10:
		tok, lit = s.scanNumber()
	default:
		s.next()
		switch ch {
		case -1:
			tok, lit = EOF, "EOF"
		case '$':
			if s.ch == '\'' {
				s.next()
				tok, lit = SYMBOL, s.scanString()
			} else {
				tok, lit = SYMBOL, s.scanIdentifier()
			}
		case '"':
			tok, lit = COMMENT, s.scanComment()
		case '\'':
			tok, lit = STRING, s.scanString()
		case ':':
			if s.ch == '=' {
				s.next()
				tok, lit = ASSIGN, ":="
			}
		case '{':
			tok, lit = LBRACE, "{"
		case '}':
			tok, lit = RBRACE, "}"
		case '(':
			tok, lit = LPAREN, "("
		case ')':
			tok, lit = RPAREN, ")"
		case ';':
			tok, lit = CASCADE, ";"
		case ',':
			tok, lit = COMMA, ","
		case '.':
			tok, lit = PERIOD, "."
		default:
			s.error(s.file.Offset(pos), fmt.Sprintf("illegal character %#U", ch))
			tok, lit = ILLEGAL, string(ch)
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

func (s *Scanner) skipWhitespace() {
	for s.ch == ' ' || s.ch == '\t' || s.ch == '\n' || s.ch == '\r' {
		s.next()
	}
}

func (s *Scanner) scanIdentifier() string {
	offs := s.offset
	for isLetter(s.ch) || s.ch == '_' {
		s.next()
	}
	return string(s.src[offs:s.offset])
}

func (s *Scanner) scanBinary() string {
	offs := s.offset
	for isBinary(s.ch) {
		s.next()
	}
	return string(s.src[offs:s.offset])
}

func isDigit(ch rune) bool {
	return '0' <= ch && ch <= '9' || ch >= 0x80 && unicode.IsDigit(ch)
}

func isLetter(ch rune) bool {
	return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch >= 0x80 && unicode.IsLetter(ch)
}

func isUpper(ch rune) bool {
	return 'A' <= ch && ch <= 'Z' || ch >= 0x80 && unicode.IsLetter(ch)
}

func isLower(ch rune) bool {
	return 'a' <= ch && ch <= 'z' || ch == '_' || ch >= 0x80 && unicode.IsLetter(ch)
}

func isBinary(ch rune) bool {
	switch ch {
	case '!', '%', '*', '/', '+', '|', '&', '^', '-', '>', '<', '=', '?', '\\', '~':
		return true
	}
	return false
}

func digitVal(ch rune) int {
	switch {
	case '0' <= ch && ch <= '9':
		return int(ch - '0')
	case 'a' <= ch && ch <= 'f':
		return int(ch - 'a' + 10)
	case 'A' <= ch && ch <= 'F':
		return int(ch - 'A' + 10)
	}
	return 16 // larger than any legal digit value
}

func (s *Scanner) scanNumber() (Token, string) {
	offs := s.offset
	s.scanMantissa(10)

	if s.ch == '.' {
		s.next()
		s.scanMantissa(10)
		return FLOAT, string(s.src[offs:s.offset])
	}
	return INT, string(s.src[offs:s.offset])
}

func (s *Scanner) scanMantissa(base int) {
	for isDigit(s.ch) || isUpper(s.ch) || isLower(s.ch) {
		if digitVal(s.ch) < base {
			s.next()
		} else {
			msg := fmt.Sprintf("illegal base %d digit '%c'", base, s.ch)
			s.error(s.offset, msg)
			return
		}
	}
}

func (s *Scanner) scanComment() string {
	offs := s.offset - 1
	for s.ch != '"' && s.ch != -1 {
		s.next()
	}

	if s.ch != '"' {
		msg := fmt.Sprintf("expecting double-quote (\") to end the comment, found EOF instead")
		s.error(s.offset, msg)
	}

	s.next()
	return string(s.src[offs:s.offset])
}

func (s *Scanner) scanString() string {
	offs := s.offset - 1
	for s.ch != '\'' && s.ch != -1 {
		s.next()
	}

	if s.ch != '\'' {
		msg := fmt.Sprintf("expecting single-quote (') to end the string, found EOF instead")
		s.error(s.offset, msg)
	}

	s.next()
	return string(s.src[offs:s.offset])
}

func (s *Scanner) error(offset int, msg string) {
	if s.err != nil {
		s.err(s.file.Position(s.file.Pos(offset)), msg)
	}
}
