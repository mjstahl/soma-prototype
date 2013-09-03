package cmd

import (
	"fmt"
	"os"
)

var EvalUsage = `Usage:
    soma eval "expression" | <file name>

    Evaluate a quoted expression or the
    contents of a specified file.

Example:
    $ soma eval "True not"
        False
    $ soma eval FooBar.soma
        .....
`

func Evaluate(args []string) {
	if len(args) < 1 {
		fmt.Println("soma eval: missing expression or file to evaluate")
		fmt.Println(EvalUsage)
		os.Exit(1)
	}

	fmt.Printf("%s\n", args)
}
