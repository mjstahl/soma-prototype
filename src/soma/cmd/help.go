package cmd

import (
	"fmt"
	"os"
)

func Help(cmd []string) {
	if len(cmd) < 1 {
		return
	}

	switch cmd[0] {
	case "console":
		fmt.Printf("%s\n", ConsoleUsage)
	case "create":
		fmt.Printf("%s\n", CreateUsage)
	case "get":
		fmt.Printf("%s\n", GetUsage)
	case "info":
		fmt.Printf("%s\n", InfoUsage)
	case "serve":
		fmt.Printf("%s\n", ServeUsage)
	case "brokers":
		fmt.Printf("%s\n", brokersHelp)
	case "libs":
		fmt.Printf("%s\n", libsHelp)
	default:
		return
	}

	os.Exit(0)
}

var brokersHelp = `
Brokers are the matchmakers between peers. Think of a broker  
(very loosely) as a combination of a BitTorrent tracker and
a gem repository. Social Machines allows a user to create a 
project and serve it as either a manifest or archive to one 
or more brokers.

A manifest is a file containing the peer IP address and object 
identifiers of the served objects. A manifest file allows a 
peer to consume the project as a utility from the project 
creator. An archive is the compressed source code of the project 
allowing a peer to host a project.

Currently  the only option available is the serving and 
consuming of project manifests.
`

var libsHelp = `
The principle followed when designing how Social Machines would 
load code and libraries into the runtime was: Load Local Later. 
Libraries and source code are loaded into a Social Machines 
runtime in the following order (assuming we are loading from 
within the 'Test' project):

    1. Test/lib/manifest.sm
    2. Test/lib/*.sa
    3. Test/src/*/*.soma
    4. Test/src/Test.soma

This principle was created because the loading of objects and 
behaviors will overwrite any objects and behaviors of that name 
with the new ones.  Therefore we wanted to guarentee any code 
created by the author would be loaded even when it would exclude 
the loading of code from a remote author.
`
