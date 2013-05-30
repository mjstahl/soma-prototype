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

// + False isNotNil => { True }
func falseIsNotNil(msg *rt.AsyncMsg) {
	returnTrue(msg)
}

// + False isNil => { False }
func falseIsNil(msg *rt.AsyncMsg) {
	returnFalse(msg)
}

// + False ifNil: nBlock => { Nil }
func falseIfNil(msg *rt.AsyncMsg) {
	returnNil(msg)
}

// + False ifNotNil: nBlock => { nBlock value }
func falseIfNotNil(msg *rt.AsyncMsg) {
	returnArgEval(msg, 2)
}

// + False ifFalse: fBlock => { fBlock value }
func falseIfFalse(msg *rt.AsyncMsg) {
	returnArgEval(msg, 2)
}

// + False ifTrue: tBlock => { Nil }
func falseIfTrue(msg *rt.AsyncMsg) {
	returnNil(msg)
}

// + False ifTrue: tBlock ifFalse: fBlock => { fBlock value }
func falseIfTrueIfFalse(msg *rt.AsyncMsg) {
	returnArgEval(msg, 2)
}

// + False not => { True }
func falseNot(msg *rt.AsyncMsg) {
	returnTrue(msg)
}

// + False & aBool => { False }
func falseAnd(msg *rt.AsyncMsg) {
	returnFalse(msg)
}

// + False | aBool => {
//   aBool ifTrue:  { True } ifFalse: { False }
// }
func falseOr(msg *rt.AsyncMsg) {
	switch msg.Args[2] {
	case rt.TRUE.OID():
		returnTrue(msg)
	case rt.FALSE.OID():
		returnFalse(msg)
	}
}

//+ False ^ aBool => {
//  aBool ifTrue: { True } ifFalse: { False }
//}
func falseXor(msg *rt.AsyncMsg) {
	switch msg.Args[2] {
	case rt.TRUE.OID():
		returnTrue(msg)
	case rt.FALSE.OID():
		returnFalse(msg)
	}
}
