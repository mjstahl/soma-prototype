// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package ast

import (
	"disco/rt"
)

type Define struct {
	Receiver string
	Behavior string
	Args     []string
	Body     *Block
}

func (d *Define) Eval(s *rt.Scope) (interface{}, error) {
	return nil, nil
}
