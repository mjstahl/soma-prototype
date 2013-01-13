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

package cmd

import (
	"disco/file"
	"disco/scan"
	"fmt"
	"os"
	"regexp"
)

var ScanUsage = `Usage:
    disco scan ("..." | *.disco)

    Tokenize the expression ("...") or file (*.disco) provided 
    as the argument.

    >>> disco scan "True not"
    === 1:1	IDEN	"True"
	1:6	IDEN	"not"

    >>> disco scan Boolean.disco
    === ...
`

func Scan(args []string) {
	if len(args) < 1 {
		fmt.Printf("%s\n", ScanUsage)
		os.Exit(0)
	}

	src := args[0]
	matches, _ := regexp.MatchString(".disco", src)
	if matches {
		fmt.Printf("Scanning file: %s\n", src)
	} else {
		expr := []byte(src)
		tokenizeExpression(expr)
	}
}

func tokenizeExpression(expr []byte) {
	var s scan.Scanner
	fset := file.NewFileSet()
	file := fset.AddFile("", fset.Base(), len(expr))
	s.Init(file, expr, nil)

	for {
		pos, tok, lit := s.Scan()
		if tok == scan.ENDF {
			break
		}
		fmt.Printf("%s\t%s\t%q\n", fset.Position(pos), tok, lit)
	}
}
