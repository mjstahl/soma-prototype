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

package test

import (
	"disco/file"
	"disco/scan"
	"testing"
)

func TestUnary(t *testing.T) {
	received := "+ True not => False."
	expected := []scan.Token{
		scan.EXTERN,
		scan.NAME,
		scan.IDENT,
		scan.DEFINE,
		scan.NAME,
		scan.PERIOD,
	}

	testTokens(t, received, expected)
}

func TestBinary(t *testing.T) {
	received := `+ True ^ aBool => 
		     aBool ifTrue: { False } ifFalse: { True }.`
	expected := []scan.Token{
		scan.EXTERN,
		scan.NAME,
		scan.BINARY,
		scan.IDENT,
		scan.DEFINE,
		scan.IDENT,
		scan.KEYWORD,
		scan.LBRACK,
		scan.NAME,
		scan.RBRACK,
		scan.KEYWORD,
		scan.LBRACK,
		scan.NAME,
		scan.RBRACK,
		scan.PERIOD,
	}

	testTokens(t, received, expected)
}

func TestKeyword(t *testing.T) {
	received := `+ True ifTrue: tBlock ifFalse: fBlock => 
		       tBlock value.`
	expected := []scan.Token{
		scan.EXTERN,
		scan.NAME,
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
