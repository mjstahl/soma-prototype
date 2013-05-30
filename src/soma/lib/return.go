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
