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
	"fmt"
	"soma/rt"
	"strings"
)

type Array struct {
	Exprs  []rt.Expr
	Values []rt.Value
}

func NewArray(a *Array, s *rt.Scope) rt.Value {
	for _, expr := range a.Exprs {
		a.Values = append(a.Values, expr.Eval(s))
	}

	obj := rt.CreateObject(a, s, 0)
	return obj
}

func (a *Array) Eval(s *rt.Scope) rt.Value {
	obj := NewArray(a, s)
	go rt.StartBehavior(obj)

	return obj
}

func (a *Array) Visit(s *rt.Scope) rt.Value {
	return a.Eval(s)
}

func (a *Array) String() string {
	if len(a.Values) == 0 {
		return "Array"
	}

	var reprs []string
	for index, repr := range a.Values {
		reprs = append(reprs, fmt.Sprintf("%d", index)+" => "+repr.String())
	}
	return fmt.Sprintf("%s", strings.Join(reprs, "\n    "))
}
