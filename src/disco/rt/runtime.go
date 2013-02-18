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

package rt

import (
	"math/rand"
)

type Runtime struct {
	Things map[OID]*Thing
	Global *Scope
	ID     uint32
}

func (r *Runtime) Init() *Runtime {
	const n = 8
	return &Runtime{make(map[OID]*Thing, n), NewScope(nil), rand.Uint32()}
}
