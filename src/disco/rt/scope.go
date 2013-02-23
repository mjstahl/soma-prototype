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
	Values map[string]uint64
}

type Heap struct {
	sync.Mutex
	Values map[uint64]Value
}

func NewScope(parent *Scope) *Scope {
	values := map[string]uint64{}

	if parent != nil {
		for key, val := range parent.Values {
			values[key] = val
		}
	}

	return &Scope{Values: values}
}

func (s *Scope) Insert(name string, oid uint64) {
	s.Lock()

	s.Values[name] = oid

	s.Unlock()
}

func (s *Scope) Lookup(name string) (oid uint64) {
	s.Lock()

	oid = s.Values[name]

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
