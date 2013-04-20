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
	"flag"
	"fmt"
	"os"
	"soma/cmd"
	_ "soma/lib"
)

var version = "0.4.0"

var usageText = `Usage: 
    soma [command] [arguments]

The commands are:

    console    interact with the Social Machines runtime
    create     create a Social Machines project
    get        retrieve a Social Machines library from a broker
    info       display Social Machines runtime information
    serve      serve a project to peers

Use "soma help [command]" for information about that command.

Additional help topics:

    brokers    on broker files and communication
    libs       how local and remote source is loaded

Use "soma help [topic]" for information about that topic.   
`

func main() {
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
	case "get":
		cmd.Get(args[1:])
	case "help":
		cmd.Help(args[1:])
		printUsage()
	case "info":
		cmd.RuntimeInfo(version)
	case "serve":
		cmd.Serve(args[1:])
	default:
		unknownCommand(args[0])
	}
}

func unknownCommand(cmd string) {
	fmt.Printf("soma: unknown command '%s'\n", cmd)
	printUsage()
}

func printUsage() {
	fmt.Println(usageText)
	os.Exit(0)
}
