package lib

import (
	"fmt"
	"soma/ast"
	"soma/rt"
)

var intBehaviorMap = map[string]primitiveFn{
	"+":  intAdd,
	"-":  intSub,
	"*":  intMul,
	"/":  intDiv,
	"==": intEqu,
}

func intAdd(msg *rt.AsyncMsg) {
	go func() {
		recv, arg := getArgIntegerValues(msg)
		result := recv + arg
		formatAndReturn(msg, result)
	}()
}

func intSub(msg *rt.AsyncMsg) {
	go func() {
		recv, arg := getArgIntegerValues(msg)
		result := recv - arg
		formatAndReturn(msg, result)
	}()
}

func intMul(msg *rt.AsyncMsg) {
	go func() {
		recv, arg := getArgIntegerValues(msg)
		result := recv * arg
		formatAndReturn(msg, result)
	}()
}

func intDiv(msg *rt.AsyncMsg) {
	go func() {
		recv, arg := getArgIntegerValues(msg)
		if arg == 0 {
			rt.NIL.Return(msg)
			return
		}
		result := recv / arg
		formatAndReturn(msg, result)
	}()
}

func intEqu(msg *rt.AsyncMsg) {
	go func() {
		recv, arg := getArgIntegerValues(msg)
		if recv == arg {
			rt.TRUE.Return(msg)
		} else {
			rt.FALSE.Return(msg)
		}
	}()
}

func getArgIntegerValues(msg *rt.AsyncMsg) (int64, int64) {
	return int64(msg.Args[0]) >> 8, int64(msg.Args[2]) >> 8
}

func formatAndReturn(msg *rt.AsyncMsg, result int64) {
	literal := fmt.Sprintf("%d", result)
	integer := ast.NewInteger(literal)
	integer.Return(msg)

	rt.INT.(*rt.Object).ID = 0x7
}
