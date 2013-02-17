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
	"bufio"
	"disco/parse"
	"fmt"
	"os"
	"strings"
)

func StartConsole(ver string) {
	fmt.Printf("Discourse language v%s. Type ':exit' to exit.\n", ver)
	for {
		fmt.Print(">>> ")
		reader := bufio.NewReader(os.Stdin)
		raw, _ := reader.ReadString('\n')
	
		input := strings.Split(raw, "\n")[0]			
		if input == ":exit" {
			os.Exit(0)
		}

		output := evaluateInput(input)
		fmt.Println("===", output)
	}
}

func evaluateInput(input string) string {
	defns, exprs := parse.ParseExpr(input)
	if defns != nil {
		return fmt.Sprintf("%#v", defns)
	}

	if exprs != nil {
		return fmt.Sprintf("%#v", exprs)
	}

	return ""
}
