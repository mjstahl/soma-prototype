// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package ast

type Expr interface {
	Visit()
}

type UnaryExpr struct {
	Receiver Expr
	Behavior string
}

func (ue *UnaryExpr) Visit() {}

type BinaryExpr struct {
	Receiver Expr
	Behavior string
	Arg      Expr
}

func (be *BinaryExpr) Visit() {}

type KeywordExpr struct {
	Receiver Expr
	Behavior string
	Args     []Expr
}

func (ke *KeywordExpr) Visit() {}
