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
	case "eval":
		fmt.Printf("%s\n", EvalUsage)
	case "get":
		fmt.Printf("%s\n", GetUsage)
	case "info":
		fmt.Printf("%s\n", InfoUsage)
	case "serve":
		fmt.Printf("%s\n", ServeUsage)
	default:
		return
	}

	os.Exit(0)
}
