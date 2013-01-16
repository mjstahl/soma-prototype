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
	"strconv"
)

type Token int

const (
	ILLEGAL Token = iota
	EOF           // EOF

	BINARY  // +, |, &
	DEFINE  // =>
	IDENT   // not
	KEYWORD // ifTrue:
	NAME    // True, False nil

	LBRACK // {
	RBRACK // }
)

var tokens = [...]string{
	ILLEGAL: "ILLEGAL",
	EOF:     "EOF",

	BINARY:  "BINARY",
	DEFINE:  "=>",
	IDENT:   "IDENT",
	KEYWORD: "KEYWORD",
	NAME:    "NAME",

	LBRACK: "{",
	RBRACK: "}",
}

func (tok Token) String() string {
	s := ""
	if 0 <= tok && tok < Token(len(tokens)) {
		s = tokens[tok]
	}

	if s == "" {
		s = strconv.Itoa(int(tok))
	}

	return s
}
