// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package ast

import (
	"disco/rt"
)

type Message interface {
	Receiver() rt.Expr
	SendAsyncMsg(rt.Value) (rt.Value, error)
	SendSyncMsg(rt.Value) (rt.Value, error)
}

type UnaryMessage struct {
	Recvr    rt.Expr
	Behavior string
}

func (ue *UnaryMessage) Eval(s *rt.Scope) (rt.Value, error) {
	return evalMessage(ue, s)
}

func (ue *UnaryMessage) Visit(s *rt.Scope) (rt.Value, error) {
	return evalMessage(ue, s)
}

func (ue *UnaryMessage) Receiver() rt.Expr { return ue.Recvr }

func (ue *UnaryMessage) SendAsyncMsg(val rt.Value) (rt.Value, error) {
	promise := rt.NewPromise()

	async := &rt.AsyncMsg{[]uint64{val.OID()}, ue.Behavior, promise.ID}
	val.Address() <- async

	return promise, nil
}

func (ue *UnaryMessage) SendSyncMsg(val rt.Value) (rt.Value, error) {
	return nil, nil
}

type BinaryMessage struct {
	Recvr    rt.Expr
	Behavior string
	Arg      rt.Expr
}

func (be *BinaryMessage) Eval(s *rt.Scope) (rt.Value, error) {
	return evalMessage(be, s)
}

func (be *BinaryMessage) Visit(s *rt.Scope) (rt.Value, error) {
	return evalMessage(be, s)
}

func (be *BinaryMessage) Receiver() rt.Expr { return be.Recvr }

func (be *BinaryMessage) SendAsyncMsg(val rt.Value) (rt.Value, error) {
	return nil, nil
}

func (be *BinaryMessage) SendSyncMsg(val rt.Value) (rt.Value, error) {
	return nil, nil
}

type KeywordMessage struct {
	Recvr    rt.Expr
	Behavior string
	Args     []rt.Expr
}

func (ke *KeywordMessage) Eval(s *rt.Scope) (rt.Value, error) {
	return evalMessage(ke, s)
}

func (ke *KeywordMessage) Visit(s *rt.Scope) (rt.Value, error) {
	return evalMessage(ke, s)
}

func (ke *KeywordMessage) Receiver() rt.Expr { return ke.Recvr }

func (ke *KeywordMessage) SendAsyncMsg(val rt.Value) (rt.Value, error) {
	return nil, nil
}

func (ke *KeywordMessage) SendSyncMsg(val rt.Value) (rt.Value, error) {
	return nil, nil
}

func evalMessage(msg Message, scope *rt.Scope) (rt.Value, error) {
	recv, lerr := msg.Receiver().Eval(scope)
	if lerr != nil {
		return nil, lerr
	}

	var promise rt.Value
	var merr error

	switch recv.OID() & 1 {
	case rt.OBJECT:
		promise, merr = msg.SendAsyncMsg(recv)
	case rt.PROMISE:
		promise, merr = msg.SendSyncMsg(recv)
	}

	return promise, merr
}
