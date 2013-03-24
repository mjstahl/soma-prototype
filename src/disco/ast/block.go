// Copyright (C) 2013 Mark Stahl

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.

// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

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

	obj := rt.CreateObject(b, 0, scope)
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
