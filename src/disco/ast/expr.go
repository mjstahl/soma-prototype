// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package ast

type Expr interface {
	Visit()
}

type UnaryMessage struct {
	Receiver Expr
	Behavior string
}

func (ue *UnaryMessage) Visit() {}

type BinaryMessage struct {
	Receiver Expr
	Behavior string
	Arg      Expr
}

func (be *BinaryMessage) Visit() {}

type KeywordMessage struct {
	Receiver Expr
	Behavior string
	Args     []Expr
}

func (ke *KeywordMessage) Visit() {}
