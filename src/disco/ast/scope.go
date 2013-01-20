// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the BSD-LICENSE file.

package ast

type Scope struct {
	Outer   *Scope
	Objects map[string]*Object
}

func CreateScope(outer *Scope) *Scope {
	const n = 4
	return &Scope{outer, make(map[string]*Object, n)}
}

func (s *Scope) Lookup(name string) *Object {
	return s.Objects[name]
}

func (s *Scope) Insert(obj *Object) (alt *Object) {
	if alt = s.Objects[obj.Name]; alt == nil {
		s.Objects[obj.Name] = obj
	}

	return
}
