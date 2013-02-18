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
	Global *Scope
	Things map[string]chan Message
}

// Scopes at this point are not concurrent themselves, and since our objects
// will be, we need to copy the values of the parent scope into the new 
// scope.
//
// Global scope is used to access any object that is of type 'ast.Name'.
func NewScope(global *Scope, parent *Scope) *Scope {
	var things map[string]chan Message
	
	if parent != nil {
		for key, val := range parent.Things {
			things[key] = val
		}
	}

	return &Scope{global, things}
}

func (s *Scope) Insert(name string, comms chan Message) {
	s.Things[name] = comms
}

func (s *Scope) Lookup(name string) chan Message {
	return s.Things[name]
}
