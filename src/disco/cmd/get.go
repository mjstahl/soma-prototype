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
//	"disco/file"
	"fmt"
	"os"
//	"os/user"
//	"path"
)

var GetUsage = `Usage:
    disco get [library url]+
    
    Retrieves a one or more library files located
    at the specified URLs.
    
    Within a project directory, a reference to the
    library will be stored in the project's 
    "lib/manifest.dm" file. Outside of a project 
    directory, the library will be appended to the 
    "~/.disco.root/lib/manifest.dm" file.
    
Example (within the Test project):
    $ disco get https://example.com/Nil.dm
	retrieve https://example.com/Nil.dm
        appended example.com/Nil.dm => Test/lib/manifest.dm
`

func Get(args []string) {
	if len(args) < 1 {
		fmt.Println("disco get: missing library url(s)")
		fmt.Println(GetUsage)
		os.Exit(1)
	}

//	pwd, _ := os.Getwd()
//	proj := file.ProjDirFrom(pwd)

//	if proj == "" {
//		user, _ := user.Current()
//		root := path.Join(user.HomeDir, ".disco.root")

//	} else {
//		root := path.Join(proj, ".disco")
//	}

//	for _, arg := range args {
//	}
}

func displayGetError(msg string, err error) {
	fmt.Printf("disco get: %s: %s\n", msg, err)
	os.Exit(1)
}
