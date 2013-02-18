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

type Thing interface {
	SendMsg(msg *Message, to *Thing)
	RecvMsg(msg *Message)
}

// ----- 32 bits -----|----- 31 bits -----|----- 1 bit -----
//      Runtime ID          Object ID             Type
// -------------------|-------------------|-----------------

type Object struct {
	ID        uint64
	Messages  []*Message
	Behaviors map[string]chan Message
}

type Promise struct {
	ID        uint64
	Messages  []*Message
	Behaviors map[string]chan Message
	Value     uint64
}
