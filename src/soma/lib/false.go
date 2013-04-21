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

func LoadFalse() {
	false := rt.CreateObject(&ast.Global{Value: "False"}, nil, 0x5)
	false.New()

	rt.RT.Globals.Insert("False", false.ID)
	rt.FALSE = false

	loadBehaviors(falseBehaviors)
}

var falseBehaviors = `
+ False isNotNil => { True }

+ False isNil => { False }

+ False ifNil => { Nil }

+ False ifNotNil: nBlock => { nBlock value }

+ False ifFalse: fBlock => { fBlock value }

+ False ifTrue: tBlock => { Nil }

+ False ifTrue: tBlock ifFalse: fBlock => { fBlock value }

+ False not => { True }

+ False & aBool => { False }

+ False | aBool => {
  aBool ifTrue:  { True } ifFalse: { False }
}

+ False ^ aBool => {
  aBool ifTrue: { True } ifFalse: { False }
}
`
