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
	case "brokers":
		fmt.Printf("%s\n", brokersHelp)
	case "libs":
		fmt.Printf("%s\n", libsHelp)
	case "objects":
		fmt.Printf("%s\n", objectsHelp)
	case "projects":
		fmt.Printf("%s\n", projectsHelp)
	default:
		return
	}

	os.Exit(0)
}

var brokersHelp = `
Brokers are the matchmakers between peers. Think of a broker  
(very loosely) as a combination of a BitTorrent tracker and
a gem repository. Discourse allows a user to create a project
and serve it as either a manifest or archive to one or more
brokers.

A manifest is a file containing the peer IP address and object 
identifiers of the served objects. A manifest file allows a 
peer to consume the project as a utility from the project 
creator. An archive is the compressed source code of the project 
allowing a peer to host a project.

Currently  the only option available is the serving and 
consuming of project manifests.

Related Commands:	
    get
    serve

Related Topics: 
    libs
    projects
`

var libsHelp = `
`

var objectsHelp = `
`

var projectsHelp = `
`
