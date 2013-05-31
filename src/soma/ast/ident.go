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
