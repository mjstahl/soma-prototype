// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package ast

import (
	"disco/rt"
)

type Ident struct {
	Name string
}

func (v *Ident) Visit(s *rt.Scope) {}

type Global struct {
	Name string
}

func (o *Global) Visit(s *rt.Scope) {}
