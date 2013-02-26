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
	scope := rt.NewScope(s)
	for _, arg := range b.Args {
		scope.Insert(arg, 0)
	}

	obj := rt.NewObject(b, scope)
	return obj
}

func (b *Block) Eval(s *rt.Scope) rt.Value {
	values := []rt.Value{}
	for _, expr := range b.Statements {
		val := expr.Eval(s)
		values = append(values, val)
	}
	
	if len(values) == 0 {
		return rt.NULL	
	}
	
	return values[len(values)-1]
}
