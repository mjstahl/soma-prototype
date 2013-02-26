// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package ast

import (
	"disco/rt"
)

type Comment struct {
	Text string
}

func (c *Comment) Eval(s *rt.Scope) rt.Value {
	return rt.NULL
}
