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

var UseUsage = `Usage:
    disco use <url of manifest>
    
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

Example:
    $ disco use https://example.com/Nil/1
        created Test/.disco/brokers/example.com
        written Test/.disco/brokers/priv.key
        written Test/.disco/brokers/pub.key
        created user @ https://example.com
        written Test/lib/Nil.dm
`

func Use(args []string) {
	if len(args) < 1 {
		DisplayUseError()
		os.Exit(1)
	}
}

func DisplayUseError() {
	fmt.Println("disco use: missing manifest url")
	fmt.Printf("%s\n", UseUsage)
}
