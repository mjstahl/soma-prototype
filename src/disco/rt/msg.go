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

package rt

type Message interface {
	ForwardMessage(Value)
}

type AsyncMsg struct {
	Args     []uint64
	Behavior string

	PromisedTo uint64
}

func (am *AsyncMsg) ForwardMessage(val Value) {
	switch val.(type) {
	case *Promise:
		promise := val.(*Promise)
		switch am.Behavior {
		case "value:":
			promise.Value = am.Args[1]
			promise.Valued <- true

		// this case will happen when a behavior is requesting
		// the value of its last expression on behalf of the 
		// original promise.  This will occurr ANY time there
		// there is a message send in a behavior body
		case "value":
			if promise.Value != 0 {
				forwardMessage(promise, am)
			} else {
				promise.Blocking = append(promise.Blocking, am)
			}
		}
	case *Object:
		obj := val.LookupBehavior(am.Behavior)
		if obj != nil {
			msg := &AsyncMsg{am.Args, "", am.PromisedTo}
			obj.Address() <- msg
		} else {
			promise := RT.Heap.Lookup(am.PromisedTo)
			async := &AsyncMsg{[]uint64{promise.OID(), NIL.OID()}, "value:", 0}
			promise.Address() <- async
		}
	}
}

type SyncMsg struct {
	Args []uint64
	Behavior string
	
	ReplyTo chan uint64
}

func (sm *SyncMsg) ForwardMessage(val Value) {
	promise := val.(*Promise)

	if promise.Value != 0 {
		forwardMessage(promise, sm)
	} else {
		promise.Blocking = append(promise.Blocking, sm)
	}
}

func ReceiveMessage(val Value, am *AsyncMsg) {
	obj := val.(*Object)
	obj.Scope.Bind(am.Args)

	ret := obj.Expr.Eval(obj.Scope)
	ret.Return(am)
}

func forwardMessage(promise *Promise, msg Message) {
	switch msg.(type) {
	case *SyncMsg:
		sm := msg.(*SyncMsg)
		reply := NewPromise().OID()
		async := &AsyncMsg{sm.Args, sm.Behavior, reply}
		oid := RT.Heap.Lookup(promise.Value)

		oid.Address() <- async
		sm.ReplyTo <- reply
	case *AsyncMsg:
		am := msg.(*AsyncMsg)
		to := RT.Heap.Lookup(am.PromisedTo)
		async := &AsyncMsg{[]uint64{to.OID(), promise.Value}, "value:", 0}
		to.Address() <- async
	}
}
