// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

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

type BinaryMessage struct {
	Receiver rt.Expr
	Behavior string
	Arg      rt.Expr
}

func (be *BinaryMessage) Eval(s *rt.Scope) rt.Value {
	return sendMessage(be.Receiver, be.Behavior, []rt.Expr{be.Arg}, s)
}

type KeywordMessage struct {
	Receiver rt.Expr
	Behavior string
	Args     []rt.Expr
}

func (ke *KeywordMessage) Eval(s *rt.Scope) rt.Value {
	return sendMessage(ke.Receiver, ke.Behavior, ke.Args, s)
}

func sendMessage(recv rt.Expr, behavior string, args []rt.Expr, scope *rt.Scope) rt.Value {
	receiver := recv.Eval(scope)
	if receiver == rt.NIL {
		return rt.NIL
	}

	oids := []uint64{receiver.OID()}
	for _, arg := range args {
		switch arg.(type) {
		case *Block:
			block := NewBlock(arg.(*Block), scope)
			oids = append(oids, block.OID())
		default:
			expr := arg.Eval(scope)
			oids = append(oids, expr.OID())
		}
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
	promise := rt.NewPromise()
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
