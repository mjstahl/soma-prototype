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

var expRecv = "Expected %s, received %s."

func TestName(t *testing.T) {
	expr := "True"
	s := initScanner(expr)

	_, tok, lit := s.Scan()
	if tok != scan.NAME {
		t.Errorf(expRecv, scan.NAME, tok)
	}

	if lit != expr {
		t.Errorf(expRecv, expr, lit)
	}
}

func TestIdentifier(t *testing.T) {
	expr := "not"
	s := initScanner(expr)

	_, tok, lit := s.Scan()
	if tok != scan.IDENT {
		t.Errorf(expRecv, scan.IDENT, tok)
	}

	if lit != expr {
		t.Errorf(expRecv, expr, lit)
	}
}

func TestKeyword(t *testing.T) {
	expr := "ifTrue:"
	s := initScanner(expr)

	_, tok, lit := s.Scan()
	if tok != scan.KEYWORD {
		t.Errorf(expRecv, scan.KEYWORD, tok)
	}

	if lit != expr {
		t.Errorf(expRecv, expr, lit)
	}
}

func TestBinary(t *testing.T) {
	expr := "+&|"
	s := initScanner(expr)
	
	_, tok, lit := s.Scan()
	if tok != scan.BINARY {
		t.Errorf(expRecv, scan.BINARY, tok)
	}

	if lit != expr {
		t.Errorf(expRecv, expr, lit)
	}
}

func TestDefine(t *testing.T) {
	expr := "=>"
	s := initScanner(expr)

	_, tok, _ := s.Scan()
	if tok != scan.DEFINE {
		t.Errorf(expRecv, scan.DEFINE, tok)
	}
}

func initScanner(expr string) scan.Scanner {
	src := []byte(expr)

	fset := file.NewFileSet()
	file := fset.AddFile("", fset.Base(), len(src))

	var s scan.Scanner
	s.Init(file, src, nil)

	return s
}
