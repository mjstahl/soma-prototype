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
		obj = rt.CreateObject(&Global{Value: d.Receiver}, nil, 0)
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
