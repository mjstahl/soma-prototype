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

import (
//"fmt"
)

// Messages come in two forms, synchronous and asynchronous. A synchronous
// message is made to a Promise and an asynchronous message is made to an 
// Object.
//
// Promises are integral to the operation of the Disource runtime.  Since 
// Disource considers the distribution first, it was important to consider
// the possibility that a Message sent to a remote object would not be 
// able to return a Promise. Therefore a distinction between Demand and
// Request was made, where a Demand is an asynchronous send, and a Request
// is a synchronous send.

// 'Args' is an array of object IDs (OIDs) representing the arguments to 
// be used by the Behavior.  Args[0] is the OID of the object receiving 
// the message (i.e. self), Args[1:n] is the remaining arguments in the
// same order dictated in the source text.
//
// 'Behavior' is the string name of the behavior to be called.
//
type Message interface {
	ForwardMessage(Value)
	ReceiveMessage(Value)
}

// AsyncMsgs are messages sent to an Object. This is an asynchronous call
// because a promise has already been created by the sender and sent
// along with the message
//
type AsyncMsg struct {
	Args     []uint64
	Behavior string

	PromisedTo uint64
}

// Promises cannot return a Promise until the value of 'Value' is not nil.
// Therefore a Message sent to a Promise will block the sender if a value
// is not available.
// 
// The means of blocking the caller is the 'ReplyTo' channel.  When a 
// Message is created by the caller, a channel is created and sent along
// with the message.  After sending the message, the caller will wait on
// on the 'ReplyTo' channel for a value or a Promise (in the case of a 
// value being available) to be returned by the receiver.
//
type SyncMsg struct {
	Args     []uint64
	Behavior string

	ReplyTo chan uint64
}

// Strip the Behavior string from the message prior
// to forwarding it on to the receiving object (the
// method to execute the message)
// This will allow us to distguish it from a message
// sent to an object and act on it differently.
//
// The below behavior is not going to last because we
// need an easier way to add "primitive" behaviors to the
// the runtime.
//
func (am *AsyncMsg) ForwardMessage(val Value) {
	switch val.(type) {
	case *Promise:
		promise := val.(*Promise)
		switch am.Behavior {
		case "value:":
			promise.Value = am.Args[1]
			promise.Valued <- true
		}
	case *Object:
		var obj Value
		switch am.Behavior {
		case "value":
			obj = RT.Heap.Lookup(am.Args[0])
		default:
			obj = val.LookupBehavior(am.Behavior)
		}

		msg := &AsyncMsg{am.Args, "", am.PromisedTo}
		obj.Address() <- msg
	}
}

func (sm *SyncMsg) ForwardMessage(val Value) {
	promise := val.(*Promise)

	if promise.Value != 0 {
		forwardMessage(promise, sm)
	} else {
		promise.Blocking = append(promise.Blocking, sm)
	}
}

func forwardMessage(promise *Promise, msg *SyncMsg) {
	reply := NewPromise()
	async := &AsyncMsg{msg.Args, msg.Behavior, reply.OID()}
	oid := RT.Heap.Lookup(promise.Value)
	oid.Address() <- async

	msg.ReplyTo <- reply.OID()
}

// At this period of time we don't need to implement
// a case for Promises as they will only have one
// behavior which is 'value:' and it will be a primitive
//
func (am *AsyncMsg) ReceiveMessage(val Value) {
	obj := val.(*Object)
	obj.Scope.Bind(am.Args)

	// still need to handle the error case, but
	// for now we will ignore it
	// it is known that lookupErrors will occur
	ret, _ := obj.Expr.Eval(obj.Scope)

	promise := RT.Heap.Lookup(am.PromisedTo)
	async := &AsyncMsg{[]uint64{promise.OID(), ret.OID()}, "value:", 0}
	promise.Address() <- async
}

func (sm *SyncMsg) ReceiveMessage(val Value) {}
