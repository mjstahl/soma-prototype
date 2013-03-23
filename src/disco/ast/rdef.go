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

package ast

import (
	"disco/rt"
)

type RDefine struct {
	Receiver string
	Behavior string
	Peer rt.Value
}

func (r *RDefine) Eval(s *rt.Scope) rt.Value {
	return nil
} 

func (r *RDefine) Visit(s *rt.Scope) rt.Value {
	return r.Eval(s)
}

func (r *RDefine) String() string {
	return r.Receiver
}