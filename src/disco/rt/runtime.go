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
	"math/rand"
	"net"
	"runtime"
)

var RT *Runtime
var NIL Value

const (
	PROMISE = 0
	OBJECT  = 1
)

type Expr interface {
	Eval(*Scope) Value
	Visit(*Scope) Value
}

type Value interface {
	Address() Mailbox
	LookupBehavior(string) Value
	OID() uint64
	Return(am *AsyncMsg)
}

type Mailbox chan Message

type Runtime struct {
	Globals *Scope
	Heap    *Heap

	IPAddr net.IP
	ID     uint64
	Procs  int
}

func InitRuntime() *Runtime {
	procs := runtime.NumCPU()
	runtime.GOMAXPROCS(procs)

	ipAddr, _ := LocalIP()

	rtid := 0 | uint64(rand.Uint32()&0xFFFFFFF0)<<32

	return &Runtime{NewScope(nil), NewHeap(), ipAddr, rtid, procs}
}

// |----- 28bits -----| ----- 28 bits ----- | ----- 7bits -----|----- 1bit -----|
// |    Runtime ID    |      Object ID      |   Primitive Type |   Object Type  |
// |------------------|---------------------|------------------|----------------|
//
func NewID(t uint64) (oid uint64) {
	for {
		obj := uint64(rand.Uint32() & 0xFFFFFFF0)
		oid = (RT.ID | (obj << 4)) | t

		if exists := RT.Heap.Lookup(oid); exists == nil {
			// reserve a spot for the new object
			RT.Heap.Insert(oid, nil)
			break
		}
	}

	return oid
}

func init() {
	RT = InitRuntime()
}
