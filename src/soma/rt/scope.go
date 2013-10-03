package rt

import (
	"fmt"
	"sync"
)

type Scope struct {
	sync.Mutex

	Values map[int]uint64
	Order  []string
}

// We should use the Parent field to copy in the scope's
// this will allow us to not have to access memory across
// goroutines, but will require an extra argument to be
// passed in to the NewScope function.
//
func NewScope(parent *Scope) *Scope {
	scope := &Scope{Values: map[int]uint64{}, Order: []string{}}
	if parent == nil {
		return scope
	}

	for index, name := range parent.Order {
		scope.Insert(name, parent.Values[index])
	}

	return scope
}

func (s *Scope) AppendScope(src *Scope) *Scope {
	if src != nil {
		for index, name := range src.Order {
			s.Insert(name, src.Values[index])
		}
	}

	return s
}

func (s *Scope) BindArguments(args []uint64) {
	s.Lock()
	defer s.Unlock()

	if len(args) > len(s.Values) {
		args = args[1:]
	}

	for i := len(args) - 1; i >= 0; i = i - 1 {
		s.Values[i] = args[i]
	}
}

func (s *Scope) Insert(name string, oid uint64) {
	_, found, index := s.Lookup(name)

	s.Lock()
	defer s.Unlock()

	if found {
		s.Values[index] = oid
		return
	}

	s.Order = append(s.Order, name)

	at := len(s.Order) - 1
	s.Values[at] = oid
}

// Lookup was created because Scopes were used to provide
// the argument environment for Blocks during execution.
// Maps are not ordered by design in Go so we needed an
// way to keep track of the order in the map.
//
func (s *Scope) Lookup(name string) (oid uint64, found bool, index int) {
	s.Lock()
	defer s.Unlock()

	index = -1
	for at, val := range s.Order {
		if val == name {
			index = at
			break
		}
	}
	if index == -1 {
		oid, found = 0, false
	} else {
		oid, found = s.Values[index], true
	}

	return
}

func (s *Scope) String() string {
	scope := make(map[string]uint64, len(s.Order))
	for index, name := range s.Order {
		scope[name] = s.Values[index]
	}

	return fmt.Sprintf("%#v", scope)
}
