// Copyright 2013 Mark Stahl. All rights reserved.
// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package scan

type Token int

const (
	ILLEGAL Token = iota
	EOF           // EOF

	COMMENT // "a comment"

	BINARY  // +, |, &, ^
	IDENT   // aBool, not
	KEYWORD // ifTrue:
	GLOBAL  // True, False nil

    ASSIGN // :=
	DEFINE // =>

	LBRACE // {
	RBRACE // }
	LPAREN // (
	RPAREN // )

	PERIOD // .
)

var tokens = [...]string{
	ILLEGAL: "ILLEGAL",
	EOF:     "EOF",

	COMMENT: "COMMENT",

	BINARY:  "BINARY",
	IDENT:   "IDENT",
	KEYWORD: "KEYWORD",
	GLOBAL:  "GLOBAL",

    ASSIGN: "ASSIGN",
	DEFINE: "DEFINE",

	LBRACE: "LBRACE",
	RBRACE: "RBRACE",
	LPAREN: "LPAREN",
	RPAREN: "RPAREN",

	PERIOD: "PERIOD",
}

func (tok Token) String() string {
	s := ""
	if 0 <= tok && tok < Token(len(tokens)) {
		s = tokens[tok]
	}

	return s
}
