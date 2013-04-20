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
	"soma/rt"
)

type Assign struct {
	Targets []string
	Exprs   []rt.Expr
}

func (a *Assign) Eval(s *rt.Scope) rt.Value {
	lvar, lval := len(a.Targets), len(a.Exprs)
	if lvar < lval {
		for index, target := range a.Targets {
			val := a.Exprs[index].Eval(s)
			s.Insert(target, val.OID())
		}
		
		for _, expr := range a.Exprs[lvar:] {
			expr.Eval(s)
		}
	} else {
		for index, expr := range a.Exprs {
			val := expr.Eval(s)
			s.Insert(a.Targets[index], val.OID())
		}

		for _, target := range a.Targets[lval:] {
			s.Insert(target, rt.NIL.OID())
		}
	}

	return nil
}

func (a *Assign) Visit(s *rt.Scope) rt.Value {
	return a.Eval(s)
}
