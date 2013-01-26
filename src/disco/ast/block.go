// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package ast

import (
	"disco/file"
)

type Block struct {
	Start file.Pos
	Exprs []Expression
}

func (b *Block) Visit() {}
func (b *Block) String() string {
	return ""
}
