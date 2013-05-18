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

type Local struct {
	Value string
}

func (l *Local) Eval(s *rt.Scope) rt.Value {
	oid, found, _ := s.Lookup(l.Value)
	if !found {
		switch l.Value {
		case "false":
			return rt.FALSE
		case "true":
			return rt.TRUE
		default:
			return rt.NIL
		}
	}

	obj := rt.RT.Heap.Lookup(oid)
	switch obj.(type) {
	case *rt.Object:
		obj.(*rt.Object).ID = oid
	}
	return obj
}

func (l *Local) Visit(s *rt.Scope) rt.Value {
	return l.Eval(s)
}

type Global struct {
	Value string
}

func (g *Global) String() string {
	return g.Value
}

func (g *Global) Eval(s *rt.Scope) rt.Value {
	oid, found, _ := rt.RT.Globals.Lookup(g.Value)
	if !found {
		return rt.NIL
	}

	obj := rt.RT.Heap.Lookup(oid)
	return obj
}

func (g *Global) Visit(s *rt.Scope) rt.Value {
	return g.Eval(s)
}
