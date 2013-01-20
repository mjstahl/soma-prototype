// Copyright 2013 Mark Stahl. All rights reserved.
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

	COMMENT // "a comment"

	BINARY  // +, |, &, ^
	IDENT   // not
	KEYWORD // ifTrue:
	NAME    // True, False nil

	DEFINE // =>

	LBRACK // {
	RBRACK // }
	PERIOD // .
)

var tokens = [...]string{
	ILLEGAL: "ILLEGAL",
	EOF:     "EOF",

	COMMENT: "COMMENT",

	BINARY:  "BINARY",
	IDENT:   "IDENT",
	KEYWORD: "KEYWORD",
	NAME:    "NAME",

	DEFINE: "=>",

	LBRACK: "{",
	RBRACK: "}",
	PERIOD: ".",
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
