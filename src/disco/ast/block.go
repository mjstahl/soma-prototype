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

func NewBlock(b *Block, s *rt.Scope) (rt.Value, error) {
	scope := rt.NewScope(s)
	for _, arg := range b.Args {
		scope.Insert(arg, 0)
	}

	obj := rt.NewObject(b, scope)
	return obj, nil
}

func (b *Block) Eval(s *rt.Scope) (rt.Value, error) {
	values := []rt.Value{}
	for _, expr := range b.Statements {
		val, err := expr.Eval(s)
		if err != nil {
			return nil, err
		}

		values = append(values, val)
	}
	
	// TODO: Issue #22
	// this will cause a panic/stack trace when the block is
	// is empty.  need a null check here to at least stop
	// the panic, but what do we do when it is empty???
	return values[len(values)-1], nil
}
