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
	"disco/file"
	"disco/parse"
	"disco/rt"
	"fmt"
	"os"
	"path"
	"path/filepath"
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
	// load files from ~/.disco.root/lib before we figure
	// out if we are in a project or not.

	pwd, _ := os.Getwd()
	pd := file.ProjDirFrom(pwd)
	if pd == "" {
		fmt.Printf("Discourse (v%s). Type ':exit' to exit.\n", ver)

		scope := rt.NewScope(nil)
		startREPL(scope)
	} else {
		scope, err := loadProject(pd)
		if err != nil {
			fmt.Printf("disco console: %s\n", err)
			os.Exit(1)
		}

		fmt.Printf("%s // Discourse (v%s). Type ':exit' to exit.\n", filepath.Base(pd), ver)
		startREPL(scope)
	}
}

func loadProject(pd string) (*rt.Scope, error) {
	src := pd + "/src"
	files, err := parse.ParseDir(file.NewFileSet(), src, isLangFile)
	if err != nil {
		return nil, err
	}

	pfile := filepath.Base(pd) + ".disco"
	s := rt.NewScope(nil)

	var last string
	for path, file := range files {
		if filepath.Base(path) != pfile {
			file.Eval(s)
		} else {
			last = path
		}
	}
	files[last].Eval(s)

	return s, nil
}

func isLangFile(info os.FileInfo) bool {
	match, _ := path.Match("*.disco", info.Name())
	return match
}

func startREPL(s *rt.Scope) {
	for {
		fmt.Printf(">>> ")
		reader := bufio.NewReader(os.Stdin)
		raw, _ := reader.ReadString('\n')

		input := strings.TrimSpace(strings.Split(raw, "\n")[0])
		if isConsoleCmd(input) {
			evalConsoleCmd(input)
		} else {
			evaluateInput(input, s)
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

func evaluateInput(input string, scope *rt.Scope) {
	expr, err := parse.ParseExpr(input)
	if err != nil {
		fmt.Println("!!!", err)
	} else {
		if len(expr) > 0 {
			fmt.Println("===", expr[0].Visit(scope))
		}
	}
}

func printNetworkInfo() {
	fmt.Println(" + Network")
	fmt.Printf(" |   IP Addr: %s\n", rt.RT.IPAddr)
	fmt.Printf(" |   Peers: %d\n", 0)
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
