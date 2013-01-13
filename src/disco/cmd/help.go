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
	case "create":
		fmt.Printf("%s\n", CreateUsage)
	case "scan":
		fmt.Printf("%s\n", ScanUsage)
	}

	os.Exit(0)
}
