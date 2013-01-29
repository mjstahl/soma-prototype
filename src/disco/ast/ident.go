// Copyright 2013 Mark Stahl. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package ast

type Variable struct {
	Name string
}

func (v *Variable) Visit() {}

type Object struct {
	Name string
}

func (o *Object) Visit() {}
