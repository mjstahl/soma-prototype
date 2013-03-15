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
	"fmt"
	"os"
)

var ServeUsage = `Usage:
disco serve <broker url>

Serves a project to the specified broker URL on port
10810.

Example (within the project 'Test'):
    $ disco serve https://example.com
        serving Test => https://example.com on 10810
`

func Serve(args []string) {
	if len(args) < 1 {
		fmt.Println("disco get: missing broker url")
		fmt.Println(ServeUsage)
		os.Exit(1)
	}
}
