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

func (ue *UnaryMessage) Eval(s *rt.Scope) (rt.Value, error) {
	return sendMessage(ue.Receiver, ue.Behavior, []rt.Expr{}, s)
}

type BinaryMessage struct {
	Receiver rt.Expr
	Behavior string
	Arg      rt.Expr
}

func (be *BinaryMessage) Eval(s *rt.Scope) (rt.Value, error) {
	return sendMessage(be.Receiver, be.Behavior, []rt.Expr{be.Arg}, s)
}

type KeywordMessage struct {
	Receiver rt.Expr
	Behavior string
	Args     []rt.Expr
}

func (ke *KeywordMessage) Eval(s *rt.Scope) (rt.Value, error) {
	return sendMessage(ke.Receiver, ke.Behavior, ke.Args, s)
}

func sendMessage(recv rt.Expr, behavior string, args []rt.Expr, scope *rt.Scope) (rt.Value, error) {
	receiver, lerr := recv.Eval(scope)
	if lerr != nil {
		// since we might have errors returned as values from message sends
		// we should go ahead and make a promise, and an error value
		// and return it so we can cut down on the number of returns
		return nil, lerr
	}

	values := []uint64{}
	for _, arg := range args {
		val, err := arg.Eval(scope)
		if err != nil {
			return nil, err
		}

		values = append(values, val.OID())
	}

	var promise rt.Value
	switch receiver.OID() & 1 {
	case rt.OBJECT:
		promise = sendAsyncMessage(receiver.Address(), behavior, values)
	case rt.PROMISE:
		promise = sendSyncMessage(receiver.Address(), behavior, values)
	}

	return promise, nil
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
