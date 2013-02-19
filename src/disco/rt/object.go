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

type Mailbox chan Message

type Thing interface {
	ReceiveMsg(msg Message, to Mailbox)
	SendMsg(msg Message, to Mailbox)
	SendAsyncMsg(msg AsyncMsg, to Mailbox)
}

type Expr interface {
	Visit(*Scope)
}

// ----- 32 bits -----|----- 31 bits -----|----- 1 bit -----
//      Runtime ID          Object ID             Type
// -------------------|-------------------|-----------------

type Object struct {
	ID uint64
	Value Expr

	Messages  []Message
	Behaviors map[string]Mailbox
}

func (o *Object) ReceiveMsg(msg Message, to Mailbox) { }
func (o *Object) SendMsg(msg Message, to Mailbox) { }
func (o *Object) SendAsyncMsg(msg AsyncMsg, to Mailbox) { }

type Promise struct {
	ID    uint64
	Value uint64

	Messages  []Message
	Behaviors map[string]Mailbox
}

func (p *Promise) ReceiveMsg(msg Message, to Mailbox) { }
func (p *Promise) SendMsg(msg Message, to Mailbox) { }
func (p *Promise) SendAsyncMsg(msg Message, to Mailbox) { }
