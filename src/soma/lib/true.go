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

func LoadTrue() {
	true := rt.CreateObject(&ast.Global{Value: "True"}, nil, 0x3)
	true.New()

	rt.RT.Globals.Insert("True", true.ID)
	rt.TRUE = true

	loadBehaviors(trueBehaviors)
}

var trueBehaviors = `
+ True isNotNil => { True }

+ True isNil => { False }

+ True ifNil: nBlock => { Nil }

+ True ifNotNil: nBlock => { nBlock value }

+ True ifFalse: fBlock => { Nil }

+ True ifTrue: tBlock => { tBlock value } 

+ True ifTrue: tBlock ifFalse: fBlock => { tBlock value }

+ True not => { False }

+ True & aBool => {
  aBool ifTrue: { True } ifFalse: { False }
}

+ True | aBool => { True }

+ True ^ aBool => {
  aBool ifTrue: { False } ifFalse: { True }
}
`
