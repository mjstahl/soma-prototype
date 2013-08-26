package main

import (
	"flag"
	"fmt"
	"os"
	"soma/cmd"
	_ "soma/lib"
)

var version = "0.5.0"

var usageText = `Usage: 
    soma [command] [arguments]

The commands are:

    console    interact with the runtime
    create     create a Social Machines project
    eval	   evalutate an expression
    get        retrieve a library from a broker
    info       display runtime information
    serve      serve a project to peers

Use "soma help [command]" for information about that command.
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
	case "eval":
		cmd.Evaluate(args[1:])
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
