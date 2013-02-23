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

func (b *Block) Eval(s *rt.Scope) (rt.Value, error) {
	values := []rt.Value{}
	for _, expr := range b.Statements {
		val, err := expr.Eval(s)
		if err != nil {
			return nil, err
		}

		values = append(values, val)
	}

	return values[len(values)-1], nil
}

func (b *Block) Visit(s *rt.Scope) (rt.Value, error) {
	scope := rt.NewScope(s)
	for _, arg := range b.Args {
		scope.Insert(arg, 0)
	}

	obj := rt.NewObject(b, scope)
	return obj, nil
}
