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

	INT
	RADIX

	ASSIGN // :=
	DEFINE // =>

	LBRACE // {
	RBRACE // }
	LPAREN // (
	RPAREN // )

	CASCADE // ;
	COMMA   // ,
	PERIOD  // .

	keyword_begin

	RETURN // return

	keyword_end
)

var tokens = [...]string{
	ILLEGAL: "ILLEGAL",
	EOF:     "EOF",

	COMMENT: "COMMENT",

	BINARY:  "BINARY",
	IDENT:   "IDENT",
	KEYWORD: "KEYWORD",
	GLOBAL:  "GLOBAL",

	INT: "INT",

	ASSIGN: ":=",
	DEFINE: "=>",

	LBRACE: "{",
	RBRACE: "}",
	LPAREN: "(",
	RPAREN: ")",

	CASCADE: ";",
	COMMA:   ",",
	PERIOD:  ".",

	RETURN: "return",
}

func (tok Token) String() string {
	s := ""
	if 0 <= tok && tok < Token(len(tokens)) {
		s = tokens[tok]
	}
	return s
}

func KeywordLookup(ident string) Token {
	if tok, is_keyword := keywords[ident]; is_keyword {
		return tok
	}
	return IDENT
}

var keywords map[string]Token

func init() {
	keywords = make(map[string]Token)
	for i := keyword_begin + 1; i < keyword_end; i++ {
		keywords[tokens[i]] = i
	}
}
