package lib

import (
	"soma/rt"
)

func returnArgEval(msg *rt.AsyncMsg, index int) {
	go func() {
		nBlock := rt.RT.Heap.Lookup(msg.Args[index])
		promise := rt.SendAsyncMessage(nBlock.Address(), "value", msg.Args)
		promise.Return(msg)
	}()
}

func returnFalse(msg *rt.AsyncMsg) {
	go func() {
		rt.FALSE.Return(msg)
	}()
}

func returnTrue(msg *rt.AsyncMsg) {
	go func() {
		rt.TRUE.Return(msg)
	}()
}

func returnNil(msg *rt.AsyncMsg) {
	go func() {
		rt.NIL.Return(msg)
	}()
}
