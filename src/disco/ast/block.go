// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package ast

import (
	"disco/rt"
)

type Block struct {
	Args       []string
	Statements []rt.Expr
}

func NewBlock(b *Block, s *rt.Scope) rt.Value {
	scope := rt.NewScope(nil)
	for _, arg := range b.Args {
		scope.Insert(arg, 0)
	}

	if s != nil {
		for at, name := range s.Order {
			scope.Insert(name, s.Values[at])
		}
	}

	obj := rt.CreateObject(b, scope)
	return obj
}

func (b *Block) Eval(s *rt.Scope) rt.Value {
	values := []rt.Value{}
	for _, expr := range b.Statements {
		val := expr.Eval(s)
		values = append(values, val)
	}

	if len(values) == 0 {
		return rt.NIL
	}
	return values[len(values)-1]
}

func (b *Block) Visit(s *rt.Scope) rt.Value {
	obj := NewBlock(b, s)
	go rt.StartBehavior(obj)

	return obj
}

func (b *Block) String() string {
	return "Block"
}
