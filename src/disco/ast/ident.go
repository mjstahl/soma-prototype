// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package ast

import (
	"disco/rt"
)

type Local struct {
	Value string
}

func (l *Local) Eval(s *rt.Scope) rt.Value {
	oid, found := s.Lookup(l.Value)
	if !found {
		return rt.NIL
	}

	obj := rt.RT.Heap.Lookup(oid)
	return obj
}

type Global struct {
	Value string
}

func (g *Global) String() string {
	return g.Value
}

func (g *Global) Eval(s *rt.Scope) rt.Value {
	oid, found := rt.RT.Globals.Lookup(g.Value)
	if !found {
		return rt.NIL
	}

	obj := rt.RT.Heap.Lookup(oid)
	return obj
}

func init() {
	null := rt.CreateObject(&Global{Value: "Nil"}, nil)
	go null.New()

	rt.RT.Globals.Insert("Nil", null.ID)
	rt.NIL = null
}
