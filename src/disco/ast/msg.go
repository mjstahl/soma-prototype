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

func (ue *UnaryMessage) Visit(s *rt.Scope) (rt.Value, error) {
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

func (be *BinaryMessage) Visit(s *rt.Scope) (rt.Value, error) {
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

func (ke *KeywordMessage) Visit(s *rt.Scope) (rt.Value, error) {
	return sendMessage(ke.Receiver, ke.Behavior, ke.Args, s)
}

func sendMessage(recv rt.Expr, behavior string, args []rt.Expr, scope *rt.Scope) (rt.Value, error) {
	receiver, lerr := recv.Eval(scope)
	if lerr != nil {
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
	var merr error
	switch receiver.OID() & 1 {
	case rt.OBJECT:
		promise, merr = sendAsyncMessage(receiver.Address(), behavior, values)
	case rt.PROMISE:
		promise, merr = sendSyncMessage(receiver.Address(), behavior, values)
	}

	return promise, merr
}

func sendAsyncMessage(recv rt.Mailbox, behavior string, args []uint64) (rt.Value, error) {
	promise := rt.NewPromise()

	async := &rt.AsyncMsg{args, behavior, promise.OID()}
	recv <- async

	return promise, nil
}

func sendSyncMessage(recv rt.Mailbox, behavior string, args []uint64) (rt.Value, error) {
	return nil, nil
}
