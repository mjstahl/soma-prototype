// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package ast

import (
	"disco/rt"
)

type Define struct {
	Receiver string
	Behavior string
	Args     []string
	Body     *Block
}

func (d *Define) Eval(s *rt.Scope) rt.Value {
	body := NewBlock(d.Body, nil)

	var obj *rt.Object
	if oid, found := rt.RT.Globals.Lookup(d.Receiver); !found {
		obj = rt.CreateObject(&Global{Value: d.Receiver}, nil)
		rt.RT.Globals.Insert(d.Receiver, obj.ID)
	} else {
		obj, _ = rt.RT.Heap.Lookup(oid).(*rt.Object)
	}

	obj.Behaviors[d.Behavior] = body.OID()

	go rt.StartBehavior(body)
	go obj.New()

	return obj
}

func (d *Define) Visit(s *rt.Scope) rt.Value {
	return d.Eval(s)
}

func (d *Define) String() string {
	return d.Receiver
}
