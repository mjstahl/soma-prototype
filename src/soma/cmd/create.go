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
	"soma/file"
)

var CreateUsage = `Usage:
    soma create <project name>
    
    Creates a Social Machines project directory as 
    a subdirectory of the current directory.

Example:
    $ create create Test
        created Test
        created Test/.soma
        created Test/lib
        created Test/lib/manifest.sm
        created Test/src
        created Test/src/Test.soma
`

func CreateProject(args []string) {
	if len(args) < 1 {
		DisplayCreateError()
		os.Exit(1)
	}

	projName := args[0]
	pwd, _ := os.Getwd()
	file.CreateProjectDir(projName, pwd)
}

func DisplayCreateError() {
	fmt.Println("soma create: missing project name argument")
	fmt.Printf("%s\n", CreateUsage)
}
