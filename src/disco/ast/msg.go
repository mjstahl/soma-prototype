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

package ast

import (
	"disco/rt"
)

type UnaryMessage struct {
	Receiver rt.Expr
	Behavior string
}

func (ue *UnaryMessage) Eval(s *rt.Scope) rt.Value {
	return sendMessage(ue.Receiver, ue.Behavior, []rt.Expr{}, s)
}

func (ue *UnaryMessage) Visit(s *rt.Scope) rt.Value {
	return ue.Eval(s)
}

type BinaryMessage struct {
	Receiver rt.Expr
	Behavior string
	Arg      rt.Expr
}

func (be *BinaryMessage) Eval(s *rt.Scope) rt.Value {
	return sendMessage(be.Receiver, be.Behavior, []rt.Expr{be.Arg}, s)
}

func (be *BinaryMessage) Visit(s *rt.Scope) rt.Value {
	return be.Eval(s)
}

type KeywordMessage struct {
	Receiver rt.Expr
	Behavior string
	Args     []rt.Expr
}

func (ke *KeywordMessage) Eval(s *rt.Scope) rt.Value {
	return sendMessage(ke.Receiver, ke.Behavior, ke.Args, s)
}

func (ke *KeywordMessage) Visit(s *rt.Scope) rt.Value {
	return ke.Eval(s)
}

func sendMessage(recv rt.Expr, behavior string, args []rt.Expr, scope *rt.Scope) rt.Value {
	receiver := recv.Visit(scope)
	
	oids := []uint64{receiver.OID()}
	for _, arg := range args {
		expr := arg.Visit(scope)
		oids = append(oids, expr.OID())
	}

	var promise rt.Value
	switch receiver.OID() & 1 {
	case rt.OBJECT:
		promise = sendAsyncMessage(receiver.Address(), behavior, oids)
	case rt.PROMISE:
		promise = sendSyncMessage(receiver.Address(), behavior, oids)
	}

	return promise
}

func sendAsyncMessage(recv rt.Mailbox, behavior string, args []uint64) rt.Value {
	promise := rt.CreatePromise()
	async := &rt.AsyncMsg{args, behavior, promise.OID()}
	recv <- async

	return promise
}

func sendSyncMessage(recv rt.Mailbox, behavior string, args []uint64) rt.Value {
	reply := make(chan uint64)
	sync := &rt.SyncMsg{args, behavior, reply}

	recv <- sync
	oid := <-reply

	return rt.RT.Heap.Lookup(oid)
}
