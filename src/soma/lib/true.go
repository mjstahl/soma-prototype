package lib

import (
	"soma/rt"
)

var trueBehaviorMap = map[string]primitiveFn{
	"isNotNil":        trueIsNotNil,
	"isNil":           trueIsNil,
	"ifNil:":          trueIfNil,
	"ifNotNil:":       trueIfNotNil,
	"ifFalse:":        trueIfFalse,
	"ifTrue:":         trueIfTrue,
	"ifTrue:ifFalse:": trueIfTrueIfFalse,
	"not":             trueNot,
	"|":               trueOr,
	"&":               trueAnd,
	"^":               trueXor,
}

// + True isNotNil => { True }
func trueIsNotNil(msg *rt.AsyncMsg) {
	returnTrue(msg)
}

// + True isNil => { False }
func trueIsNil(msg *rt.AsyncMsg) {
	returnFalse(msg)
}

// + True ifNil: nBlock => { Nil }
func trueIfNil(msg *rt.AsyncMsg) {
	returnNil(msg)
}

// + True ifNotNil: nBlock => { nBlock value }
func trueIfNotNil(msg *rt.AsyncMsg) {
	returnArgEval(msg, 2)
}

// + True ifFalse: fBlock => { Nil }
func trueIfFalse(msg *rt.AsyncMsg) {
	returnNil(msg)
}

// + True ifTrue: tBlock => { tBlock value }
func trueIfTrue(msg *rt.AsyncMsg) {
	returnArgEval(msg, 2)
}

// + True ifTrue: tBlock ifFalse: fBlock => { tBlock value }
func trueIfTrueIfFalse(msg *rt.AsyncMsg) {
	returnArgEval(msg, 2)
}

// + True not => { False }
func trueNot(msg *rt.AsyncMsg) {
	returnFalse(msg)
}

// + True | aBool => { True }
func trueOr(msg *rt.AsyncMsg) {
	returnTrue(msg)
}

// + True & aBool => {
//   aBool ifTrue: { True } ifFalse { False }
// }
func trueAnd(msg *rt.AsyncMsg) {
	switch msg.Args[2] {
	case rt.TRUE.OID():
		returnTrue(msg)
	case rt.FALSE.OID():
		returnFalse(msg)
	}
}

// + True ^ aBool => {
//   aBool ifTrue: { False } ifFalse { True }
// }
func trueXor(msg *rt.AsyncMsg) {
	switch msg.Args[2] {
	case rt.TRUE.OID():
		returnFalse(msg)
	case rt.FALSE.OID():
		returnTrue(msg)
	}
}
