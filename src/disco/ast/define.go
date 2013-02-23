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

// Defines are never evaluated because they are only visited
// in the top scope, and cannot be defined in the context
// of a Block.
//
func (d *Define) Eval(s *rt.Scope) (rt.Value, error) {
	return d.Visit(s)
}

func (d *Define) Visit(s *rt.Scope) (rt.Value, error) {
	body, err := d.Body.Visit(nil)
	if err != nil {
		return nil, err
	}

	var obj *rt.Object
	if oid, found := rt.RT.Globals.Lookup(d.Receiver); !found {
		obj = rt.NewObject(&Global{Value: d.Receiver}, nil)
		rt.RT.Globals.Insert(d.Receiver, obj.ID)
	} else {
		obj, _ = rt.RT.Heap.Lookup(oid).(*rt.Object)
	}

	if obj.Behaviors == nil {
		obj.Behaviors = map[string]rt.Value{d.Behavior: body}
	} else {
		obj.Behaviors[d.Behavior] = body
	}

	go rt.StartBehavior(body)
	go rt.StartObject(obj)

	return obj, nil
}

func (d *Define) String() string {
	return d.Receiver
}
