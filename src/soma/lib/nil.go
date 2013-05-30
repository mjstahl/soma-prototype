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
	"soma/ast"
	"soma/rt"
)

func LoadNil() {
	null := rt.CreateObject(&ast.Global{Value: "Nil"}, nil, 0x1)
	null.New()

	rt.RT.Globals.Insert("Nil", null.ID)
	rt.NIL = null

	nilBehaviorObj := rt.CreateObject(nil, nil, 0)
	startPrimitiveBehaviors(null, nilBehaviorObj, nilBehaviorMap)
}

var nilBehaviorMap = map[string]primitiveFn{
	"isNil":      nilIsNil,
	"isNotNil":   nilIsNotNil,
	"ifNil:":     nilifNil,
	"ifNotNil::": nilIfNotNil,
}

// + Nil isNil => { True }
func nilIsNil(msg *rt.AsyncMsg) {
	go func() {
		rt.TRUE.Return(msg)
	}()
}

// + Nil isNotNil => { False }
func nilIsNotNil(msg *rt.AsyncMsg) {
	go func() {
		rt.FALSE.Return(msg)
	}()
}

// + Nil ifNil: nBlock => { nBlock value }
func nilifNil(msg *rt.AsyncMsg) {
	go func() {
		nBlock := rt.RT.Heap.Lookup(msg.Args[2])
		promise := rt.SendAsyncMessage(nBlock.Address(), "value", msg.Args)
		promise.Return(msg)
	}()
}

// + Nil ifNotNil: nBlock => { Nil }
func nilIfNotNil(msg *rt.AsyncMsg) {
	go func() {
		rt.NIL.Return(msg)
	}()
}
