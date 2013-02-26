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
	"bufio"
	"disco/parse"
	"disco/rt"
	"fmt"
	"os"
	"runtime"
	"strings"
)

var ConsoleUsage = `Usage:
    disco console

    Start a discourse read-eval-print loop
    allowing the user to evaluate expressions.

Example:
    $ disco console
    >>> + True not => { False }
    >>> ...
    >>> True not
    === False

The disource console supports commands that 
are evaluated differently than discourse 
expressions.

The commands are:
   :exit		exits the discourse console
   :info		displays runtime information
   :objs		list of objects and behaviors
`

func StartConsole(ver string) {
	fmt.Printf("Discourse language v%s. Type ':exit' to exit.\n", ver)

	scope := rt.NewScope(nil)
	for {
		fmt.Print(">>> ")
		reader := bufio.NewReader(os.Stdin)
		raw, _ := reader.ReadString('\n')

		input := strings.TrimSpace(strings.Split(raw, "\n")[0])
		if isConsoleCmd(input) {
			evalConsoleCmd(input)
		} else {
			expr := evaluateInput(input, scope)
			fmt.Println("===", expr)
		}
	}
}

func isConsoleCmd(input string) bool {
	if input[0] == ':' {
		return true
	}

	return false
}

func evalConsoleCmd(input string) {
	lower := strings.ToLower(input)
	switch lower {
	case ":exit":
		os.Exit(0)
	case ":info":
		printRuntimeInfo()
		printNetworkInfo()
		printMemoryInfo()
	case ":objs":
		printObjects()
	}
}

func printNetworkInfo() {
	fmt.Println(" + Network")
	fmt.Printf(" |   IP Addr: %s\n", rt.RT.IPAddr)
}

func printRuntimeInfo() {
	fmt.Println(" + Runtime")
	fmt.Printf(" |   Processors (Used/Avail): %d/%d\n", rt.RT.Procs, runtime.NumCPU())
	fmt.Printf(" |   ID: 0x%x\n", rt.RT.ID>>31)

	named := len(rt.RT.Globals.Values)
	heap := len(rt.RT.Heap.Values)
	goroutines := runtime.NumGoroutine()
	fmt.Printf(" |   Objects (Named/Lang/Sys): %d/%d/%d\n", named, heap, goroutines)
}

func printMemoryInfo() {
	mem := new(runtime.MemStats)
	runtime.ReadMemStats(mem)

	fmt.Println(" + Memory")
	fmt.Printf(" |   Total Allocated: %d KB\n", mem.TotalAlloc/1024)
	fmt.Printf(" |   In Use: %d KB\n", mem.Alloc/1024)

	avg := mem.PauseTotalNs / uint64(mem.NumGC) / 1.0e3
	fmt.Printf(" |   Avg. GC Pause: %d \u03BCs\n", avg)
}

func printObjects() {
	for index, name := range rt.RT.Globals.Order {
		fmt.Printf(" + %s\n", name)

		oid := rt.RT.Globals.Values[index]
		obj := rt.RT.Heap.Values[oid]
		for behave, _ := range obj.(*rt.Object).Behaviors {
			fmt.Printf(" |   %s\n", behave)
		}
	}
}

func evaluateInput(input string, scope *rt.Scope) rt.Value {
	expr := parse.ParseExpr(input)[0]
	return expr.Eval(scope)
}
