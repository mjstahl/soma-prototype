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

package main

import (
	"disco/cmd"
	"disco/file"
	"flag"
	"fmt"
	"os"
)

var version = "0.3.0"

var discoCmdUsageText = `Usage: 
    disco [command] [arguments]

The commands are:
  - broker		manage discourse brokers
  - console		interact with the discourse runtime
    create		create a discourse project
  - get			retrieve a discourse archive
    info		display discourse runtime information
  - serve		serve a project to peers
  - use			retrieve a discourse manifest

Use "disco help [command]" for more information about that command. 

Commands marked with '-' are not yet complete.
`

func main() {
	file.CreateDiscoRoot()

	flag.Parse()
	args := flag.Args()
	if len(args) < 1 {
		printUsage()
	}

	switch args[0] {
	case "console":
		cmd.StartConsole(version)
	case "create":
		cmd.CreateProject(args[1:])
	case "help":
		cmd.Help(args[1:])
		printUsage()
	case "info":
		cmd.RuntimeInfo(version)
	default:
		unknownCommand(args[0])
	}
}

func unknownCommand(cmd string) {
	fmt.Printf("disco: unknown command '%s'\n", cmd)
	printUsage()
}

func printUsage() {
	fmt.Println(discoCmdUsageText)
	os.Exit(0)
}
