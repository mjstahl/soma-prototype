package scan

type Token int

const (
	ILLEGAL Token = iota
	EOF           // EOF

	COMMENT // "a comment"

	BINARY  // '!', '%', '*', '/', '+', '|', '&', '^', '-', '>', '<', '=', '?', '\', '~':
	GETTER  // @attribute
	GLOBAL  // True, False, Nil
	IDENT   // aBool, not
	KEYWORD // ifTrue:
	SETTER  // @attribute:

	INT

	ASSIGN // :=
	DEFINE // =>

	LBRACE // {
	RBRACE // }
	LPAREN // (
	RPAREN // )

	CASCADE // ;
	COMMA   // ,
	PERIOD  // .
)

var tokens = [...]string{
	ILLEGAL: "ILLEGAL",
	EOF:     "EOF",

	COMMENT: "COMMENT",

	BINARY:  "BINARY",
	GETTER:  "GETTER",
	GLOBAL:  "GLOBAL",
	IDENT:   "IDENT",
	KEYWORD: "KEYWORD",
	SETTER:  "SETTER",

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
}

func (tok Token) String() string {
	s := ""
	if 0 <= tok && tok < Token(len(tokens)) {
		s = tokens[tok]
	}
	return s
}
