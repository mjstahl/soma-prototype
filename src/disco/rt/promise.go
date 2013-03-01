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
	"time"
)

type Promise struct {
	ID   uint64
	Addr Mailbox

	Value  uint64
	Valued chan bool

	Behaviors map[string]Value

	Blocking []Message
}

func CreatePromise() *Promise {
	id := NewID(PROMISE)

	n := 128
	promise := &Promise{ID: id, Addr: make(Mailbox, n), Behaviors: map[string]Value{}, Blocking: []Message{}}
	promise.Valued = make(chan bool, 1)

	RT.Heap.Insert(id, promise)
	go promise.New()

	return promise
}

func (promise *Promise) New() {
	for {
		select {
		case <-promise.Valued:
			for _, msg := range promise.Blocking {
				forwardMessage(promise, msg)
			}
			promise.Blocking = []Message{}
		case msg := <-promise.Address():
			msg.ForwardMessage(promise)
		}
	}
}

func (p *Promise) Return(am *AsyncMsg) {
	async := &AsyncMsg{[]uint64{}, "value", am.PromisedTo}
	p.Address() <- async
}

func (p *Promise) String() string {
	for p.Value == 0 {
		time.Sleep(10 * time.Millisecond)
	}

	obj := RT.Heap.Lookup(p.Value).(*Object)
	return fmt.Sprintf("%s (0x%x @ %s)", obj.Expr, (obj.ID & 0x7FFFFFFF), RT.IPAddr)
}

func (p *Promise) OID() uint64 {
	return p.ID
}

func (p *Promise) Address() Mailbox {
	return p.Addr
}

func (p *Promise) LookupBehavior(name string) Value {
	return p.Behaviors[name]
}
