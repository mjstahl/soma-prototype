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
    $ soma create Test
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
