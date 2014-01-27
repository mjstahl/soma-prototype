package ast

import (
	"soma/rt"
)

type Define struct {
	Receiver string
	Behavior string
	Body     *Block
}

func (d *Define) Eval(s *rt.Scope) rt.Value {
	body := NewBlock(d.Body, s)

	var obj *rt.Object
	if oid, found, _ := rt.RT.Globals.Lookup(d.Receiver); !found {
		obj = rt.CreateObject(&Global{Value: d.Receiver}, nil, 0)
		rt.RT.Globals.Insert(d.Receiver, obj.ID)
	} else {
		obj, _ = rt.RT.Heap.Lookup(oid).(*rt.Object)
	}

	obj.Behaviors[d.Behavior] = body.OID()

	rt.StartBehavior(body)
	obj.New()

	return obj
}

func (d *Define) Visit(s *rt.Scope) rt.Value {
	return d.Eval(s)
}

func (d *Define) String() string {
	return d.Receiver
}
