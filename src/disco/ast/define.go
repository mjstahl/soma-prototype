// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package ast

type DefineType int

const (
	EXT DefineType = iota
)

type Define struct {
	Type     DefineType // External, Internal
	Receiver string     // True, False, Nil
	Behavior string     // not, ifTrue:ifFalse:
	Args     []string   // tBlock, fBlock
	Exprs    []Expr
}

func (d *Define) Visit() {}
