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
	"disco/rt"
)

type RDefine struct {
	Receiver string
	OID uint64
	Behavior string
	Peers    []*rt.Peer
}

// TODO(mjs) Right now multiple peers are not handled for a given object
// this will have to get fixed once source code archives can also be 
// posted to a broker.
//
func (r *RDefine) Eval(s *rt.Scope) rt.Value {
	var obj *rt.Object
	var start = false
	if oid, found := rt.RT.Globals.Lookup(r.Receiver); !found {
		obj = rt.CreateObject(&Global{Value: r.Receiver}, nil)
		rt.RT.Globals.Insert(r.Receiver, obj.ID)

		go obj.New()
		start = true
	} else {
		obj, _ = rt.RT.Heap.Lookup(oid).(*rt.Object)
	}

	o := rt.RT.Heap.Lookup(obj.Behaviors[r.Behavior])
	if o == nil {
		obj.Behaviors[r.Behavior] = r.Peers[0].OID()

		start = true
	}

	if start {
		go r.Peers[0].New()
	}

	return obj
}

func (r *RDefine) Visit(s *rt.Scope) rt.Value {
	return r.Eval(s)
}

func (r *RDefine) String() string {
	return r.Receiver
}
