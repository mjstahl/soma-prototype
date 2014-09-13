package lib

import (
	"soma/rt"
)

var nilBehaviorMap = map[string]primitiveFn{
	"isNil":     nilIsNil,
	"isNotNil":  nilIsNotNil,
	"ifNil:":    nilifNil,
	"ifNotNil:": nilIfNotNil,
}

// + Nil isNil -> True.
//
func nilIsNil(msg *rt.AsyncMsg) {
	returnTrue(msg)
}

// + Nil isNotNil -> False.
//
func nilIsNotNil(msg *rt.AsyncMsg) {
	returnFalse(msg)
}

// + Nil ifNil: nBlock -> nBlock value.
//   (0)          (1)
func nilifNil(msg *rt.AsyncMsg) {
	returnArgEval(msg, 1)
}

// + Nil ifNotNil: nBlock -> Nil.
//
func nilIfNotNil(msg *rt.AsyncMsg) {
	returnNil(msg)
}
