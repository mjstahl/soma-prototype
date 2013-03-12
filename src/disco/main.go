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

var version = "0.2.0"

var usageText = `Usage: 
    disco [command] [arguments]

The commands are:

    console    interact with the discourse runtime
    create     create a discourse project
  - get        retrieve a discourse library from a broker
    info       display discourse runtime information
  - serve      serve a project to peers

Use "disco help [command]" for information about that command.

Additional help topics:

  - brokers    broker files and communication
  - libs       how third-party libraries are loaded
  - objects    discourse's view of objects
  - projects   the role and processing of projects 

Use "disco help [topic]" for information about that topic.   
`

func main() {
	file.CreateRootDir()

	flag.Parse()
	args := flag.Args()
	if len(args) < 1 {
		printDiscoUsage()
	}

	switch args[0] {
	case "console":
		cmd.StartConsole(version)
	case "create":
		cmd.CreateProject(args[1:])
	case "get":
		cmd.Get(args[1:])
	case "help":
		cmd.Help(args[1:])
		printDiscoUsage()
	case "info":
		cmd.RuntimeInfo(version)
	default:
		unknownCommand(args[0])
	}
}

func unknownCommand(cmd string) {
	fmt.Printf("disco: unknown command '%s'\n", cmd)
	printDiscoUsage()
}

func printDiscoUsage() {
	fmt.Println(usageText)
	os.Exit(0)
}
