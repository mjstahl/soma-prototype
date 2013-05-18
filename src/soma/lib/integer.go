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
	"fmt"
	"soma/ast"
	"soma/rt"
)

func LoadIntegers() {
	integer := rt.CreateObject(&ast.Global{Value: "Integer"}, nil, 0x7)
	integer.New()

	behaviors := rt.CreateObject(nil, nil, 0)
	for name, _ := range behaviorMap {
		integer.Behaviors[name] = behaviors.OID()
	}
	startIntBehaviors(behaviors)

	rt.RT.Globals.Insert("Integer", integer.ID)
	rt.INTEGER = integer
}

func startIntBehaviors(behaviors rt.Value) {
	go func() {
		for {
			msg := <-behaviors.Address()
			amsg := msg.(*rt.AsyncMsg)
			if behaviorFn, fn := behaviorMap[amsg.Behavior]; !fn {
				(rt.NIL).Return(amsg)
			} else {
				behaviorFn(amsg, int64(amsg.Args[0])>>8, int64(amsg.Args[2])>>8)
			}
		}
	}()
}

type intFn func(*rt.AsyncMsg, int64, int64)

var behaviorMap = map[string]intFn{
	"+": intAdd,
	"-": intSub,
	"*": intMul,
	"/": intDiv,
}

func intAdd(msg *rt.AsyncMsg, recv int64, arg int64) {
	go func() {
		result := recv + arg
		formatAndReturn(msg, result)
	}()
}

func intSub(msg *rt.AsyncMsg, recv int64, arg int64) {
	go func() {
		result := recv - arg
		formatAndReturn(msg, result)
	}()
}

func intMul(msg *rt.AsyncMsg, recv int64, arg int64) {
	go func() {
		result := recv * arg
		formatAndReturn(msg, result)
	}()
}

func intDiv(msg *rt.AsyncMsg, recv int64, arg int64) {
	go func() {
		if arg == 0 || recv == 0 {
			(rt.NIL).Return(msg)
			return
		}
		result := recv / arg
		formatAndReturn(msg, result)
	}()
}

func formatAndReturn(msg *rt.AsyncMsg, result int64) {
	literal := fmt.Sprintf("%d", result)
	integer := ast.NewInteger(literal)
	integer.Return(msg)
}
