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

		// TODO(mjs) Refactor 'value' and 'value:' out of ForwardMessage
		// into a Promise primitives library
		//
		switch am.Behavior {
		case "value:":
			promise.Value = am.Args[2]
			promise.Valued <- true

		// This case will happen when a behavior is requesting
		// the value of its last expression on behalf of the 
		// original promise.  This will occurr ANY time there
		// there is a message send in a behavior body
		//
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
			am.Args[1] = obj.OID()

			msg := &AsyncMsg{am.Args, "given:", am.PromisedTo}
			obj.Address() <- msg
		} else {
			// If we can't find the behavior, then we need to send 'Nil' to
			// the waiting Promise.
			//
			promise := RT.Heap.Lookup(am.PromisedTo)
			async := &AsyncMsg{[]uint64{0, 0, promise.OID(), NIL.OID()}, "value:", 0}
			promise.Address() <- async
		}
	}
}

type SyncMsg struct {
	Args     []uint64
	Behavior string

	ReplyTo chan uint64
}

// If the Promise has yet to receive a value, hold on to 
// Message until a value is received.  If the Promise has
// a value then go ahead and forward the message on to the
// Object.
//
func (sm *SyncMsg) ForwardMessage(val Value) {
	promise := val.(*Promise)

	if promise.Value != 0 {
		forwardMessage(promise, sm)
	} else {
		promise.Blocking = append(promise.Blocking, sm)
	}
}

// This is called when a Promise has received a value and is
// forwarding it on to that value.  

// Synchronous messages are received by promised by are not sent 
// to Objects, therefore if we have received a synchronous we 
// need to convert it to an asynchronous message before sending 
// it on to the object.
//
// Asynchronous messages are received by Blocks (literals, or
// definition bodies) when setting the value returned or requested
// by any body that contains message sends.  The body will request
// the value and request that it be forwarded on to the original
// Promise.
// 
func forwardMessage(promise *Promise, msg Message) {
	switch msg.(type) {
	case *SyncMsg:
		sm := msg.(*SyncMsg)
		reply := CreatePromise().OID()
		async := &AsyncMsg{sm.Args, sm.Behavior, reply}
		oid := RT.Heap.Lookup(promise.Value)

		oid.Address() <- async
		sm.ReplyTo <- reply
	case *AsyncMsg:
		am := msg.(*AsyncMsg)
		to := RT.Heap.Lookup(am.PromisedTo)

		var recv uint64
		switch to.(type) {
		case *Promise:
			recv = to.OID()
		case *Peer:
			recv = am.PromisedTo
		}
		async := &AsyncMsg{[]uint64{recv, 0, promise.Value}, "value:", 0}
		to.Address() <- async
	}
}

func SendMessage(recv Expr, behavior string, args []Expr, scope *Scope) Value {
	receiver := recv.Visit(scope)

	// [self (the object), this (the behavior), args...]
	// Since we have not yet determined the behavior we need to store a place 
	// for it which is always args[1].  It will get set in ForwardMessage.
	oids := []uint64{receiver.OID(), 0}

	for _, arg := range args {
		expr := arg.Visit(scope)
		oids = append(oids, expr.OID())
	}

	var promise Value
	switch receiver.OID() & 1 {
	case OBJECT:
		promise = sendAsyncMessage(receiver.Address(), behavior, oids)
	case PROMISE:
		promise = sendSyncMessage(receiver.Address(), behavior, oids)
	}

	return promise
}

func sendAsyncMessage(recv Mailbox, behavior string, args []uint64) Value {
	promise := CreatePromise()
	async := &AsyncMsg{args, behavior, promise.OID()}
	recv <- async

	return promise
}

func sendSyncMessage(recv Mailbox, behavior string, args []uint64) Value {
	reply := make(chan uint64)
	sync := &SyncMsg{args, behavior, reply}

	recv <- sync
	oid := <-reply

	return RT.Heap.Lookup(oid)
}

// Currently ReceiveMessage is started as a goroutine used
// by Block literals and a Behavior body (they are effectively
// the same).
//
func ReceiveMessage(val Value, am *AsyncMsg) {
	obj := val.(*Object)

	// This scope binding will be used for 'value', and 'given:',
	// 'where:' will require a different binding method because
	// its argument will be hashmap.
	//
	obj.Scope.BindOrder(am.Args)

	ret := obj.Expr.Eval(obj.Scope)
	ret.Return(am)
}
