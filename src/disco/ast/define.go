// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package ast

import (
	"disco/file"
)

type DefineType int

const (
	EXT DefineType = iota
)

type Define struct {
	Start	 file.Pos
	Type     DefineType // External, Internal
	Receiver string     // True, False, Nil
	Behavior string     // not, ifTrue:ifFalse:
	Args     []string   // tBlock, fBlock
	//Body   []*Expression
}
