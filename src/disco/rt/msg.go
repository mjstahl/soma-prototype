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
	ReceiveMessage(Value) Value
}

// AsyncMsgs are messages sent to an Object. This is an asynchronous call
// because a promise has already been created by the sender and sent
// along with the message
//
type AsyncMsg struct {
	Args []uint64
	Behavior string

	PromisedTo uint64
}

func (am *AsyncMsg) ReceiveMessage(val Value) Value {
	return nil
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
	Args []uint64
	Behavior string

	ReplyTo chan uint64
}

func (sm *SyncMsg) ReceiveMessage(val Value) Value {
	return nil
}
