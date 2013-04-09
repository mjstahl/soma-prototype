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

package lib

import (
	"soma/ast"
	"soma/rt"
)

func LoadNil() {
	null := rt.CreateObject(&ast.Global{Value: "Nil"}, nil, 0x1)
	go null.New()

	rt.RT.Globals.Insert("Nil", null.ID)
	rt.NIL = null

	loadBehaviors(nilBehaviors)
}

var nilBehaviors = `
+ Nil isNil => { True }

+ Nil isNotNil => { False }

+ Nil ifNil: nBlock => { nBlock value }

+ Nil ifNotNil: nBlock => { Nil }
`
