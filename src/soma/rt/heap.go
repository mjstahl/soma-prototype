package rt

import (
	"sync"
)

type Heap struct {
	sync.Mutex
	Values map[uint64]Value
}

func NewHeap() *Heap {
	return &Heap{Values: map[uint64]Value{}}
}

func (h *Heap) Insert(oid uint64, val Value) {
	h.Lock()
	defer h.Unlock()

	h.Values[oid] = val
}

// If we don't find it in the local runtime heap it could
// be a remote object in which case we need to look up
// the object ID in the Peers map.
//
func (h *Heap) Lookup(oid uint64) Value {
	h.Lock()
	defer h.Unlock()

	switch oid & 0xF {
	case 0x7:
		return INT
	}

	val := h.Values[oid]
	if val != nil {
		return val
	}

	return h.lookupPeer(oid)
}

func (h *Heap) lookupPeer(oid uint64) Value {
	// this should look up the object in the heap that is
	// only made up of the top 28 bits of an object ID
	// (i.e. a runtime id)
	// we need this because when a promise on a remote
	// is looked up it is a full object id and won't be
	// found... but what we really need is just the runtime
	// id so it can find a *Peer
	id := (oid >> 36) << 36
	peer := RT.Peers[id]
	if peer != nil {
		peer.ID = oid
		return peer
	}

	return nil
}
