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
	return &Scope{Values: map[int]uint64{}, Order: []string{}}
}

func (s *Scope) Bind(objs []uint64) {
	s.Lock()

	for at, val := range objs {
		s.Values[at] = val
	}

	s.Unlock()
}

func (s *Scope) Insert(name string, oid uint64) {
	s.Lock()

	s.Order = append(s.Order, name)

	at := len(s.Order) - 1
	s.Values[at] = oid

	s.Unlock()
}

// Lookup was created because Scopes were used to provide
// the argument environment for Blocks during execution.
// Maps are not ordered by design in Go so we needed an
// way to keep track of the order in the map.
//
func (s *Scope) Lookup(name string) (oid uint64, found bool) {
	s.Lock()

	index := -1
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

	s.Unlock()

	return
}

func NewHeap() *Heap {
	return &Heap{Values: map[uint64]Value{}}
}

func (h *Heap) Insert(oid uint64, val Value) {
	h.Lock()

	h.Values[oid] = val

	h.Unlock()
}

func (h *Heap) Lookup(oid uint64) (val Value) {
	h.Lock()

	val = h.Values[oid]

	h.Unlock()

	return
}
