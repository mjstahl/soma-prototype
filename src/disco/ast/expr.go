// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package ast

import (
	"disco/file"
)

type Expression interface {
	Visit()
}

type Expr struct {
	Start    file.Pos
	Receiver Expression
	Behavior string
	Args     []Expression
}

func (e *Expr) Visit() {}
func (e *Expr) String() string {
	return ""
}

type Block struct {
	Start file.Pos
	Exprs []Expression
	End   file.Pos
}

func (e *Block) Visit() {}
func (e *Block) String() string {
	return ""
}
