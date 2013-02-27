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
		if am.Behavior == "value" {
			obj := val.(*Object)
			promise := RT.Heap.Lookup(am.PromisedTo)
			// this is problematic... could be a promise.. and we
			// then have the same problem we have down in ReceiveMessage
			value := obj.Expr.Eval(obj.Scope)
			async := &AsyncMsg{[]uint64{promise.OID(), value.OID()}, "value:", 0}
			promise.Address() <- async
		} else {

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
}

func (sm *SyncMsg) ForwardMessage(val Value) {
	promise := val.(*Promise)

	if promise.Value != 0 {
		forwardMessage(promise, sm)
	} else {
		promise.Blocking = append(promise.Blocking, sm)
	}
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
	ret := obj.Expr.Eval(obj.Scope)

	switch ret.(type) {
	case *Object:
		promise := RT.Heap.Lookup(am.PromisedTo)
		async := &AsyncMsg{[]uint64{promise.OID(), ret.OID()}, "value:", 0}
		promise.Address() <- async

	// 'ret' will be a promise if there are ANY message sends
	// inside a behaviors block expr.  The runtime can not return
	// a promise to the requesting promise as they could be on
	// different machines.  Therefore behavior requests the value
	// of the promise (of the last expr) on behalf of the original
	// promise.
	case *Promise:
		async := &AsyncMsg{[]uint64{obj.OID()}, "value", am.PromisedTo}
		ret.(*Promise).Address() <- async
	}
}

func (sm *SyncMsg) ReceiveMessage(val Value) {}
