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
	scope := rt.NewScope(s)
	for _, arg := range b.Args {
		scope.Insert(arg, 0)
	}	
	
	obj := rt.NewObject(b, scope)
	return obj, nil
}
