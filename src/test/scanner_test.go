package test

import (
	"soma/file"
	"soma/scan"
	"testing"
)

func TestUnary(t *testing.T) {
	received := "+ True not -> False."
	expected := []scan.Token{
		scan.BINARY,
		scan.GLOBAL,
		scan.IDENT,
		scan.DEFINE,
		scan.GLOBAL,
		scan.PERIOD,
	}
	testTokens(t, received, expected)
}

func TestBinary(t *testing.T) {
	received := 
		`+ True ^ aBool -> 
		   aBool ifTrue: { False } ifFalse: { True }.`
	expected := []scan.Token{
		scan.BINARY,
		scan.GLOBAL,
		scan.BINARY,
		scan.IDENT,
		scan.DEFINE,
		scan.IDENT,
		scan.KEYWORD,
		scan.LBRACE,
		scan.GLOBAL,
		scan.RBRACE,
		scan.KEYWORD,
		scan.LBRACE,
		scan.GLOBAL,
		scan.RBRACE,
		scan.PERIOD,
	}
	testTokens(t, received, expected)
}

func TestKeyword(t *testing.T) {
	received := 
		`+ True ifTrue: tBlock ifFalse: fBlock ->
		   tBlock value.`
	expected := []scan.Token{
		scan.BINARY,
		scan.GLOBAL,
		scan.KEYWORD,
		scan.IDENT,
		scan.KEYWORD,
		scan.IDENT,
		scan.DEFINE,
		scan.IDENT,
		scan.IDENT,
		scan.PERIOD,
	}
	testTokens(t, received, expected)
}

func initScanner(expr string) scan.Scanner {
	src := []byte(expr)

	fset := file.NewFileSet()
	file := fset.AddFile("", fset.Base(), len(src))

	var s scan.Scanner
	s.Init(file, src, nil)

	return s
}

func testTokens(t *testing.T, expr string, tokens []scan.Token) {
	s := initScanner(expr)
	msg := "Expected (%s) -- Received (%s)\n"

	for _, token := range tokens {
		_, tok, _ := s.Scan()
		if tok != token {
			t.Fatalf(msg, token, tok)
		}
	}
}
