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
	"disco/parse"
	"fmt"
	"os"
)

var ParseUsage = `Usage:
    disco parse '[expression]'

    Parse the expression string provided as an argument.

Example:
    $ disco parse '+ False & aBool => { False }'
	DEFN   False   &   ( aBool )
	   BODY   { }
	      NAME   False
`

func Parse(args []string) {
	if len(args) < 1 {
		displayParseError()
		os.Exit(1)
	}

	defns, exprs := parse.ParseExpr(args[0])

	fmt.Println("Defns:\n")
	fmt.Printf("%#v", defns)

	fmt.Println("Exprs:\n")
	fmt.Printf("%#v", exprs)
}

func displayParseError() {
	fmt.Printf("disco parse: missing expression string\n")
	fmt.Printf("%s\n", ParseUsage)
}
