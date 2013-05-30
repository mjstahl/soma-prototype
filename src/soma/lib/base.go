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
	"soma/parse"
	"soma/rt"
)

type primitiveFn func(*rt.AsyncMsg)

func init() {
	rt.NIL = loadPrimitiveObj("Nil", 0x01, nilBehaviorMap)
	rt.TRUE = loadPrimitiveObj("True", 0x03, trueBehaviorMap)
	rt.FALSE = loadPrimitiveObj("False", 0x05, falseBehaviorMap)
	rt.INT = loadPrimitiveObj("Integer", 0x07, intBehaviorMap)
}

func loadPrimitiveObj(name string, id uint64, behaviorMap map[string]primitiveFn) rt.Value {
	obj := rt.CreateObject(&ast.Global{Value: name}, nil, id)
	obj.New()

	behaviorObj := rt.CreateObject(nil, nil, 0)
	startPrimitiveBehaviors(obj, behaviorObj, behaviorMap)

	rt.RT.Globals.Insert(name, obj.ID)
	return obj
}

func startPrimitiveBehaviors(recv *rt.Object, behaviorObj rt.Value, behaviorMap map[string]primitiveFn) {
	for name, _ := range behaviorMap {
		recv.Behaviors[name] = behaviorObj.OID()
	}

	go func() {
		for {
			msg := <-behaviorObj.Address()
			amsg := msg.(*rt.AsyncMsg)
			if behaviorFn, fn := behaviorMap[amsg.Behavior]; !fn {
				(rt.NIL).Return(amsg)
			} else {
				behaviorFn(amsg)
			}
		}
	}()
}

func loadBehaviors(src string) {
	exprs, _ := parse.ParseExpr(src)
	for _, expr := range exprs {
		expr.Visit(nil)
	}
}
