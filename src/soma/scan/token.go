package scan

type Token int

const (
	ILLEGAL Token = iota
	EOF           // EOF

	COMMENT // "a comment"

	BINARY  // '!', '%', '*', '/', '+', '|', '&', '^', '-', '>', '<', '=', '?', '\', '~':
	KEYWORD // ifTrue:
	IDENT   // aBool, not
	GLOBAL  // True, False nil
	STRING  // "a string"
	SYMBOL  // $symbol, $'a symbol'
	ATTR    // @firstName

	INT
	FLOAT

	ASSIGN // :=
	DEFINE // =>

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

	BINARY:  "BINARY",
	KEYWORD: "KEYWORD",
	IDENT:   "IDENT",
	GLOBAL:  "GLOBAL",
	STRING:  "STRING",
	SYMBOL:  "SYMBOL",
	ATTR:    "ATTR",

	INT:   "INT",
	FLOAT: "FLOAT",

	ASSIGN: ":=",
	DEFINE: "=>",

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
