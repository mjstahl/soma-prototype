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
	
	// this is REALLY bad because we are going to 
	// be doing lookups across goroutines
	// we have to fix this by figuring out a different
	// way to do Bind()
	Parent *Scope
	
	Values map[int]uint64
	Order []string
}

type Heap struct {
	sync.Mutex
	Values map[uint64]Value
}

func NewScope(parent *Scope) *Scope {
	return  &Scope{Values: map[int]uint64{}, Order: []string{}, Parent: parent}
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
	values := map[uint64]Value{}

	return &Heap{Values: values}
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
