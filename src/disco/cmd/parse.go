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
	"disco/parse"
	"fmt"
	"os"
)

var ParseUsage = `Usage:
    disco parse "[expression]"

    Parse the expression string provided as an argument.

Example:
    $ disco parse "True not"
`

func Parse(args []string) {
	if len(args) < 1 {
		displayParseError()
		os.Exit(1)
	}

	src := []byte(args[0])
	parseExpression(src)
}

func displayParseError() {
	fmt.Printf("disco parse: missing expression string\n")
	fmt.Printf("%s\n", ParseUsage)
}

func parseExpression(expr []byte) {
	fset := file.NewFileSet()
	file := fset.AddFile("", fset.Base(), len(expr))

	var p parse.Parser
	p.Init(file, expr, true)
	p.Parse()	
}
