package ast

import (
	"soma/rt"
)

type RemoteObject struct {
	Receiver string
	RID      uint64
	Behavior string
	BID      uint64
	Peers    []*rt.Peer
}

// TODO(mjs) Right now multiple peers are not handled for a given object
// this will have to get fixed once source code archives can also be
// posted to a broker.
//
func (r *RemoteObject) Eval(s *rt.Scope) rt.Value {
	var obj *rt.Object
	var start = false
	if oid, found, _ := rt.RT.Globals.Lookup(r.Receiver); !found {
		obj = rt.CreateObject(&Global{Value: r.Receiver}, nil, r.RID)
		rt.RT.Globals.Insert(r.Receiver, r.RID)

		obj.New()
		start = true
	} else {
		obj, _ = rt.RT.Heap.Lookup(oid).(*rt.Object)
	}

	o := rt.RT.Heap.Lookup(obj.Behaviors[r.Behavior])
	if o == nil {
		obj.Behaviors[r.Behavior] = r.BID

		start = true
	}

	if start {
		r.Peers[0].New()
	}

	return obj
}

func (r *RemoteObject) Visit(s *rt.Scope) rt.Value {
	return r.Eval(s)
}

func (r *RemoteObject) String() string {
	return r.Receiver
}
