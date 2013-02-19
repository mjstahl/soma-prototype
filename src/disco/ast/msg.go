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

func (ue *UnaryMessage) Visit(s *rt.Scope) {}

type BinaryMessage struct {
	Receiver rt.Expr
	Behavior string
	Arg      rt.Expr
}

func (be *BinaryMessage) Visit(s *rt.Scope) {}

type KeywordMessage struct {
	Receiver rt.Expr
	Behavior string
	Args     []rt.Expr
}

func (ke *KeywordMessage) Visit(s *rt.Scope) {}
