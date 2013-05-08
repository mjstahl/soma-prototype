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
	"strconv"
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
				behaviorFn(amsg, int64(amsg.Args[0]), int64(amsg.Args[2]))
			}
		}
	}()
}

type intFn func(*rt.AsyncMsg, int64, int64)

var behaviorMap = map[string]intFn{
	"+": int_plus,
}

func int_plus(msg *rt.AsyncMsg, recv int64, arg int64) {
	go func() {
		base := int((recv >> 4) & 0xF)
		arg1, arg2 := recv>>8, arg>>8
		literal := fmt.Sprintf("%d#%s", base, strconv.FormatInt(arg1+arg2, base))
		result := ast.NewInteger(literal)
		result.Return(msg)
	}()
}
