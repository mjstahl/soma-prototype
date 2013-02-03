// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package ast

type Expr interface {
	Visit()
}

type Message interface {
	Expr
	SetReceiver(recv Expr)
}

type UnaryMessage struct {
	Receiver Expr
	Behavior string
}

func (ue *UnaryMessage) SetReceiver(recv Expr) {
	ue.Receiver = recv
}

func (ue *UnaryMessage) Visit() {}

type BinaryMessage struct {
	Receiver Expr
	Behavior string
	Arg      Expr
}

func (be *BinaryMessage) SetReceiver(recv Expr) {
	be.Receiver = recv
}

func (be *BinaryMessage) Visit() {}

type KeywordMessage struct {
	Receiver Expr
	Behavior string
	Args     []Expr
}

func (ke *KeywordMessage) SetReceiver(recv Expr) {
	ke.Receiver = recv
}

func (ke *KeywordMessage) Visit() {}
