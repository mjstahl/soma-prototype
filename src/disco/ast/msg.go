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

type UnaryMessage struct {
	Receiver rt.Expr
	Behavior string
}

func (ue *UnaryMessage) Eval(s *rt.Scope) rt.Value {
	return rt.SendMessage(ue.Receiver, ue.Behavior, []rt.Expr{}, s)
}

func (ue *UnaryMessage) Visit(s *rt.Scope) rt.Value {
	return ue.Eval(s)
}

type BinaryMessage struct {
	Receiver rt.Expr
	Behavior string
	Arg      rt.Expr
}

func (be *BinaryMessage) Eval(s *rt.Scope) rt.Value {
	return rt.SendMessage(be.Receiver, be.Behavior, []rt.Expr{be.Arg}, s)
}

func (be *BinaryMessage) Visit(s *rt.Scope) rt.Value {
	return be.Eval(s)
}

type KeywordMessage struct {
	Receiver rt.Expr
	Behavior string
	Args     []rt.Expr
}

func (ke *KeywordMessage) Eval(s *rt.Scope) rt.Value {
	return rt.SendMessage(ke.Receiver, ke.Behavior, ke.Args, s)
}

func (ke *KeywordMessage) Visit(s *rt.Scope) rt.Value {
	return ke.Eval(s)
}
