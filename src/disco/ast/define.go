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

func (d *Define) Eval(s *rt.Scope) (rt.Value, error) {
	body, err := d.Body.Eval(nil)
	if err != nil {
		return nil, err
	}

	var obj *rt.Object
	if oid := rt.RT.Globals.Lookup(d.Receiver); oid == 0 {
		obj = rt.NewObject(&Global{Value: d.Receiver})
		rt.RT.Globals.Insert(d.Receiver, obj.ID)
	} else {
		obj, _ = rt.RT.Heap.Lookup(oid).(*rt.Object)
	}

	if obj.Behaviors == nil {
		obj.Behaviors = map[string]rt.Value{d.Behavior: body}
	} else {
		obj.Behaviors[d.Behavior] = body
	}

	go rt.StartObject(body)
	go rt.StartObject(obj)

	return obj, nil
}

func (d *Define) String() string {
	return d.Receiver
}
