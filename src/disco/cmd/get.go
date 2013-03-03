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

var GetUsage = `Usage:
    disco get <url>
    
    Retrieves a manifest file located at the
    specified URL.
    
    Within a project directory, the file will
    be stored in the project's ".lib" directory.  
    Outside of a project directory, the manifest 
    will be stored in the "~/.disco.root/lib" 
    directory.
    
    If broker keys have not yet been generated 
    for the broker, they will be generated upon
    the execution of this command. 

Example (within the Test project):
    $ disco get https://example.com/1/Nil.dm
        created .disco/brokers/example.com
        written .disco/brokers/example.com/pub.key
        written .disco/brokers/example.com/priv.key
        created user @ https://example.com
        written lib/Nil.dm
`

func Get(args []string) {
	if len(args) < 1 {
		DisplayGetError()
		os.Exit(1)
	}
}

func DisplayGetError() {
	fmt.Println("disco use: missing manifest url")
	fmt.Printf("%s\n", GetUsage)
}
