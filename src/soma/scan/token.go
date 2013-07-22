package scan

type Token int

const (
	ILLEGAL Token = iota
	EOF           // EOF

	COMMENT // "a comment"

	BINARY  // '!', '%', '*', '/', '+', '|', '&', '^', '-', '>', '<', '=', '?', '\', '~':
	IDENT   // aBool, not
	KEYWORD // ifTrue:
	GLOBAL  // True, False nil
	STRING  // "a string"
	SYMBOL  // $symbol, $'a symbol'

	INT

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

	// keyword_begin

	// RETURN // return

	// keyword_end
)

var tokens = [...]string{
	ILLEGAL: "ILLEGAL",
	EOF:     "EOF",

	COMMENT: "COMMENT",

	BINARY:  "BINARY",
	IDENT:   "IDENT",
	KEYWORD: "KEYWORD",
	GLOBAL:  "GLOBAL",
	STRING:  "STRING",
	SYMBOL:  "SYMBOL",

	INT: "INT",

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

