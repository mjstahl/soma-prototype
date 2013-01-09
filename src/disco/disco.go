// Copyright (C) 2013 Mark Stahl

package main

import (
	"flag"
	"fmt"
	"os"
	"os/user"
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
		createProjectDir(args[1:])
	case "info":
		printRuntimeInfo()
	}
}

func discoRootDoesNotExist() bool {
	user, _ := user.Current()
	rootDir := user.HomeDir + "/.disco.root"

	if _, err := os.Stat(rootDir); err != nil {
		if os.IsNotExist(err) {
			return true
		}
	}

	return false
}

func createDiscoRoot() {
	user, _ := user.Current()

	rootDir := user.HomeDir + "/.disco.root"
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
    + info		display discourse runtime information
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

	projDir := pwd + "/" + projName
	err := os.Mkdir(projDir, 0700)
	if err != nil {
		fmt.Printf("error creating project directory: %s\n", err)
		os.Exit(0)
	}

	fmt.Printf("    create %s/%s\n", projName, ".disco.proj")

	projFile := pwd + "/" + projName + "/.disco.proj"
	file, err := os.OpenFile(projFile, os.O_CREATE, 0700)
	if err != nil {
		fmt.Printf("error creating project root: %s\n", err)
		os.Exit(0)
	}

	file.Close()

	fmt.Printf("    create %s/%s\n", projName, "dist")

	projDistDir := projDir + "/dist"
	err = os.Mkdir(projDistDir, 0700)
	if err != nil {
		fmt.Printf("error creating project distribution directory: %s\n", err)
		os.Exit(0)
	}

	fmt.Printf("    create %s/%s\n", projName, "src")

	projSrcDir := projDir + "/src"
	err = os.Mkdir(projSrcDir, 0700)
	if err != nil {
		fmt.Printf("error creating project source directory: %s\n", err)
	}
}

func printRuntimeInfo() {
	fmt.Printf("Discourse version %s\n", version)
}
