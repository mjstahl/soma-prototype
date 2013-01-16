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
)

var InfoUsage = `Usage: 
    disco info

    Displays information about the Discourse 
    language runtime.  If in a project directory
    the output includes information specific
    to the project.

Example:
    $ disco info
    Discourse Language v0.1.0
    Copyright (C) 2013 Mark Stahl

    Portions of this source code are:
    Copyright (C) 2012 The Go Authors
`

func RuntimeInfo(ver string) {
	fmt.Printf("Discourse Language v%s\n", ver)
	fmt.Printf("Copyright (C) 2013 Mark Stahl\n")
	fmt.Printf("\n")
	fmt.Printf("Portions of this source code are:\n")
	fmt.Printf("Copyright (C) 2012 The Go Authors\n")
}
