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

package rt

import (
	"fmt"
	"sync"
)

type Scope struct {
	sync.Mutex

	Values map[int]uint64
	Order  []string
}

type Heap struct {
	sync.Mutex
	Values map[uint64]Value
}

// We should use the Parent field to copy in the scope's
// this will allow us to not have to access memory across
// goroutines, but will require an extra argument to be
// passed in to the NewScope function.
//
func NewScope(parent *Scope) *Scope {
	scope := &Scope{Values: map[int]uint64{}, Order: []string{}}
	if parent == nil {
		return scope
	}

	for index, name := range parent.Order {
		scope.Insert(name, parent.Values[index])
	}

	return scope
}

func (s *Scope) AppendScope(src *Scope) *Scope {
	if src != nil {
		for index, name := range src.Order {
			s.Insert(name, src.Values[index])
		}
	}

	return s
}

func (s *Scope) BindOrder(objs []uint64) {
	s.Lock()
	defer s.Unlock()

	for at, val := range objs {
		s.Values[at] = val
	}
}

func (s *Scope) Insert(name string, oid uint64) {
	_, found, index := s.Lookup(name)

	s.Lock()
	defer s.Unlock()

	if found {
		s.Values[index] = oid
		return
	}

	s.Order = append(s.Order, name)

	at := len(s.Order) - 1
	s.Values[at] = oid
}

// Lookup was created because Scopes were used to provide
// the argument environment for Blocks during execution.
// Maps are not ordered by design in Go so we needed an
// way to keep track of the order in the map.
//
func (s *Scope) Lookup(name string) (oid uint64, found bool, index int) {
	s.Lock()
	defer s.Unlock()

	index = -1
	for at, val := range s.Order {
		if val == name {
			index = at
			break
		}
	}
	if index == -1 {
		oid, found = 0, false
	} else {
		oid, found = s.Values[index], true
	}

	return
}

func (s *Scope) String() string {
	scope := make(map[string]uint64, len(s.Order))
	for index, name := range s.Order {
		scope[name] = s.Values[index]
	}

	return fmt.Sprintf("%#v", scope)
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
		return INTEGER
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
