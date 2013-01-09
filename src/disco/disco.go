package main

import (
	"flag"
	"fmt"
	"os"
)

var version = "0.1.0"

func main() {
	flag.Parse()

	args := flag.Args()
	if len(args) < 1 {
		printUsage()
	}

	switch args[0] {
	default:
		unknownCommand(args[0])
	case "create":
		createProject(args[1:])
	case "version":
		printVersion()
	}
}

func unknownCommand(cmd string) {
	fmt.Printf("disco: unknown command '%s'\n", cmd)
	printUsage()
}

var usageText = 
`Usage: 
    disco [command] [arguments]

The commands are:
    create		create a discourse project
    version		print the discourse version
`

func printUsage() {
	fmt.Println(usageText)
	os.Exit(0)
}

func createProject(args []string) {
	cwd, _ := os.Getwd()
	fmt.Printf("creating '%s' project at '%s'\n", args[0], cwd)
}

func printVersion() {
	fmt.Printf("discourse version %s\n", version)
}

