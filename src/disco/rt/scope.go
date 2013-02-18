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

type Scope struct {
	Parent *Scope
	Things map[string]OID
}

func NewScope(parent *Scope) *Scope {
	const n = 4
	return &Scope{parent, make(map[string]OID, n)}
}

func (s *Scope) Lookup(name string) OID {
	return s.Things[name]
}

func (s *Scope) Insert(name string, object OID) OID {
	if exists := s.Things[name]; exists == 0 {
		s.Things[name] = object
	}

	return object
}
