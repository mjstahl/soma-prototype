package rt

import (
	"fmt"
)

type Object struct {
	ID   uint64
	Addr Mailbox

	Expr Expr

	Scope     *Scope
	Behaviors map[string]uint64
}

func CreateObject(val Expr, scope *Scope, id uint64) *Object {
	var oid uint64
	if id != 0 {
		oid = id
	} else {
		oid = NewID(OBJECT)
	}

	n := 128
	obj := &Object{ID: oid, Expr: val, Scope: scope, Addr: make(Mailbox, n), Behaviors: map[string]uint64{}}

	RT.Heap.Insert(oid, obj)
	return obj
}

func (obj *Object) New() {
	go func() {
		for {
			msg := <-obj.Address()
			msg.ForwardMessage(obj)
		}
	}()
}

// This will be called when the last expression of a Block is an identifier
// or a global. If the last expression is a message it will call "Return" on
// Promise.  Returning an object creates an asynchronous "value:" and sends it
// to the sender's Promise, setting the value.
//
func (o *Object) Return(am *AsyncMsg) {
	promise := RT.Heap.Lookup(am.PromisedTo)
	async := &AsyncMsg{[]uint64{promise.OID(), 0, o.OID()}, "value:", 0}
	promise.Address() <- async
}

func (o *Object) String() string {
	id := o.ID & 0xFFFFFFFFF
	if len(RT.Peers) > 0 {
		return fmt.Sprintf("%s (0x%x @ %s)", o.Expr, id, RT.IPAddr)
	} else {
		return fmt.Sprintf("%s", o.Expr)
	}
}

func (o *Object) OID() uint64 {
	return o.ID
}

func (o *Object) Address() Mailbox {
	return o.Addr
}

func (o *Object) LookupBehavior(name string) Value {
	oid := o.Behaviors[name]
	obj := RT.Heap.Lookup(oid)

	if obj != nil {
		return obj
	}

	return nil
}

// StartBehavior is used to spinup a goroutine for a Definition body, or a
// Block literal.  In the case of either, we don't need to forward on because
// we are receiving a forwarded message from a Global, or we are going to be
// calling a block primitive, which right now does not have a way to extend
// its Behaviors.
//
func StartBehavior(obj Value) {
	go func() {
		for {
			msg := <-obj.Address()
			am := msg.(*AsyncMsg)
			ReceiveMessage(obj, am)
		}
	}()
}
