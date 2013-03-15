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
	"disco/rt"
	"fmt"
	"os"
	"path/filepath"
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
		displayServeError("missing broker url", nil)
	}

	pwd, _ := os.Getwd()
	pd := file.ProjDirFrom(pwd)
	if pd == "" {
		displayServeError("must be called within discourse project", nil)
	}

	scope := rt.NewScope(nil)
	rs, err := LoadRootDir(scope)
	if err != nil {
		displayConsoleError("failed to load discourse root", err)
	}

	LoadProjectDir(pd, rs)
	if err != nil {
		displayServeError("failed to load project directory", err)
	}

	pname := filepath.Base(pd)
	fmt.Printf("    serving %s => %s on 10810\n", pname, args[0])
	startServingLoop()
}

func startServingLoop() {
	for {

	}
}

func displayServeError(msg string, err error) {
	if err != nil {
		fmt.Printf("disco serve: %s: %s\n", msg, err)
	} else {
		fmt.Printf("disco serve: %s\n", msg)
	}

	os.Exit(1)
}
