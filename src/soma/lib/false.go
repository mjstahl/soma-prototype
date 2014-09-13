package lib

import (
	"soma/rt"
)

var falseBehaviorMap = map[string]primitiveFn{
	"isNotNil":        falseIsNotNil,
	"isNil":           falseIsNil,
	"ifNil:":          falseIfNil,
	"ifNotNil:":       falseIfNotNil,
	"ifFalse:":        falseIfFalse,
	"ifTrue:":         falseIfTrue,
	"ifTrue:ifFalse:": falseIfTrueIfFalse,
	"not":             falseNot,
	"&":               falseAnd,
	"|":               falseOr,
	"^":               falseXor,
}

// + False isNotNil -> True.
//
func falseIsNotNil(msg *rt.AsyncMsg) {
	returnTrue(msg)
}

// + False isNil -> False.
//
func falseIsNil(msg *rt.AsyncMsg) {
	returnFalse(msg)
}

// + False ifNil: nBlock -> Nil.
//
func falseIfNil(msg *rt.AsyncMsg) {
	returnNil(msg)
}

// + False ifNotNil: nBlock -> nBlock value.
//    (0)              (1)
func falseIfNotNil(msg *rt.AsyncMsg) {
	returnArgEval(msg, 1)
}

// + False ifFalse: fBlock -> fBlock value.
//	  (0)            (1)
func falseIfFalse(msg *rt.AsyncMsg) {
	returnArgEval(msg, 1)
}

// + False ifTrue: tBlock -> Nil.
//
func falseIfTrue(msg *rt.AsyncMsg) {
	returnNil(msg)
}

// + False ifTrue: tBlock ifFalse: fBlock -> fBlock value.
//    (0)            (1)            (2)
func falseIfTrueIfFalse(msg *rt.AsyncMsg) {
	returnArgEval(msg, 2)
}

// + False not -> True.
//
func falseNot(msg *rt.AsyncMsg) {
	returnTrue(msg)
}

// + False & aBool -> False.
//
func falseAnd(msg *rt.AsyncMsg) {
	returnFalse(msg)
}

// + False | aBool ->
//   aBool ifTrue:  { True } ifFalse: { False }.
//
func falseOr(msg *rt.AsyncMsg) {
	switch msg.Args[1] {
	case rt.TRUE.OID():
		returnTrue(msg)
	case rt.FALSE.OID():
		returnFalse(msg)
	}
}

// + False ^ aBool ->
//   aBool ifTrue: { True } ifFalse: { False }.
//
func falseXor(msg *rt.AsyncMsg) {
	switch msg.Args[1] {
	case rt.TRUE.OID():
		returnTrue(msg)
	case rt.FALSE.OID():
		returnFalse(msg)
	}
}