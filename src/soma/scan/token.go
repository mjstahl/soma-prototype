package scan

type Token int

const (
	ILLEGAL Token = iota
	EOF           // EOF

	COMMENT // "a comment"
	STRING  // 'a string'

	BINARY  // '!', '%', '*', '/', '+', '|', '&', '^', '-', '>', '<', '=', '?', '\', '~':
	GETTER  // @attribute
	GLOBAL  // True, False, Nil
	IDENT   // aBool, not
	KEYWORD // ifTrue:
	SETTER  // @attribute:
	SYMBOL  // $%, $True, $aBool

	INT

	ASSIGN // :=
	DEFINE // ->

	LBRACE // {
	RBRACE // }
	LBRACK // [
	RBRACK // ]
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
	STRING:  "STRING",

	BINARY:  "BINARY",
	GETTER:  "GETTER",
	GLOBAL:  "GLOBAL",
	IDENT:   "IDENT",
	KEYWORD: "KEYWORD",
	SETTER:  "SETTER",
	SYMBOL:  "SYMBOL",

	INT: "INT",

	ASSIGN: ":=",
	DEFINE: "->",

	LBRACE: "{",
	RBRACE: "}",
	LBRACK: "[",
	RBRACK: "]",
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
