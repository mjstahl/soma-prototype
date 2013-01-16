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

var HelpUsage = `Usage:
    disco help [topic | command]

    Displays the usage of the provided topic
    or command.

    Valid commands are:
        create
        info
        scan
`

func Help(cmd []string) {
	if len(cmd) < 1 {
		fmt.Printf("%s\n", HelpUsage)
		os.Exit(0)
	}

	switch cmd[0] {
	case "create":
		fmt.Printf("%s\n", CreateUsage)
	case "info":
		fmt.Printf("%s\n", InfoUsage)
	case "scan":
		fmt.Printf("%s\n", ScanUsage)
	}

	os.Exit(0)
}
