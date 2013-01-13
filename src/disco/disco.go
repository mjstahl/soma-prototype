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
	"flag"
	"fmt"
	"os"
	"os/user"
	"path"
)

var version = "0.1.0"

func main() {
	if discoRootDoesNotExist() {
		createDiscoRoot()
	}

	flag.Parse()

	args := flag.Args()
	if len(args) < 1 {
		printUsage()
	}

	switch args[0] {
	default:
		unknownCommand(args[0])
	case "create":
		// cmd.CreateProject(args[1:])
		createProjectDir(args[1:])
	case "help":
		if len(args) < 2 {
			printUsage()
		}
		cmd.CommandHelp(args[1])
	case "info":
		// cmd.RuntimeInfo()
		printRuntimeInfo()
	case "scan":
		cmd.Scan(args[1:])
	}
}

func discoRootDoesNotExist() bool {
	user, _ := user.Current()
	rootDir := path.Join(user.HomeDir, "/.disco.root")

	if _, err := os.Stat(rootDir); err != nil {
		if os.IsNotExist(err) {
			return true
		}
	}

	return false
}

func createDiscoRoot() {
	user, _ := user.Current()

	rootDir := path.Join(user.HomeDir, "/.disco.root")
	err := os.Mkdir(rootDir, 0700)
	if err != nil {
		fmt.Printf("error creating ~/.disco.root: %s", err)
	}
}

func unknownCommand(cmd string) {
	fmt.Printf("disco: unknown command '%s'\n", cmd)
	printUsage()
}

var discoCmdUsageText = `Usage: 
    disco [command] [arguments]

The commands are:
    - add-broker	add broker to discourse project or root
    - console		interact with the discourse runtime
    + create		create a discourse project
    - get		retrieve a discourse archive
    - help		display help for a disco command
    + info		display discourse runtime information
    - scan		lexically analyze discourse source
    - serve		serve a project to peers
    - use		retrieve a discourse manifest

Commands marked with '+' are ready for use.
`

func printUsage() {
	fmt.Println(discoCmdUsageText)
	os.Exit(0)
}

func createProjectDir(args []string) {
	if len(args) < 1 {
		// should display the information for
		// 'disco create' not the 'disco' command
		// usage
		printUsage()
	}

	projName := args[0]
	pwd, _ := os.Getwd()

	fmt.Printf("    create %s/\n", projName)

	projDir := path.Join(pwd, projName)
	err := os.Mkdir(projDir, 0700)
	if err != nil {
		fmt.Printf("error creating project directory: %s\n", err)
		os.Exit(0)
	}

	projFileName := path.Join(pwd, projName, ".disco.proj")
	fmt.Printf("    create %s/%s\n", projName, ".disco.proj")

	file, err := os.OpenFile(projFileName, os.O_CREATE, 0700)
	defer file.Close()
	if err != nil {
		fmt.Printf("error creating project root: %s\n", err)
		os.Exit(0)
	}

	fmt.Printf("    create %s/%s\n", projName, "dist")

	projDistDir := path.Join(projDir, "dist")
	err = os.Mkdir(projDistDir, 0700)
	if err != nil {
		fmt.Printf("error creating project distribution directory: %s\n", err)
		os.Exit(0)
	}

	fmt.Printf("    create %s/%s\n", projName, "src")

	projSrcDir := path.Join(projDir, "src")
	err = os.Mkdir(projSrcDir, 0700)
	if err != nil {
		fmt.Printf("error creating project source directory: %s\n", err)
		os.Exit(0)
	}

	fmt.Printf("    create %s/%s/%s\n", projName, "src", projName+".disco")

	projMainFile := path.Join(projSrcDir, projName+".disco")
	mainFile, err := os.OpenFile(projMainFile, os.O_CREATE, 0700)
	defer mainFile.Close()
	if err != nil {
		fmt.Printf("error creating main project file: %s\n", err)
		os.Exit(0)
	}
}

func printRuntimeInfo() {
	fmt.Printf("Discourse Language v%s\n", version)
	fmt.Printf("Copyright (C) 2013 Mark Stahl\n")
	fmt.Printf("Licensed under the GNU AGPL v3\n")
}
