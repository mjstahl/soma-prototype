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
	"bytes"
	"encoding/json"
	"fmt"
	"net"
	"net/http"
)

type Peer struct {
	Addr   Mailbox
	ID     uint64
	IPAddr net.IP
	Port   int
}

func CreatePeer(ip net.IP, port int, rid uint64) *Peer {
	n := 128
	peer := &Peer{Addr: make(Mailbox, n), ID: rid, IPAddr: ip, Port: port}
	RT.Peers[rid] = peer

	return peer
}

func (p *Peer) Address() Mailbox {
	return p.Addr
}

type RemoteMsg struct {
	Port      int
	RuntimeID uint64
	Msg       *AsyncMsg
}

func (p *Peer) ForwardMessage(msg Message) {
	ipaddr := fmt.Sprintf("%s:%d", p.IPAddr, p.Port)
	url := fmt.Sprintf("http://%s/msg", ipaddr)

	rmsg := &RemoteMsg{Port: RT.Port, RuntimeID: RT.ID, Msg: msg.(*AsyncMsg)}

	json, _ := json.Marshal(rmsg)
	body := bytes.NewBuffer(json)

	http.Post(url, "text/json", body)
}

// This method will never occur because all remote objects are created
// local IDs.  Therefore *Object.LookupBehavior will always be performed
//
func (p *Peer) LookupBehavior(name string) Value {
	return p
}

func (p *Peer) New() {
	for {
		msg := <-p.Address()
		p.ForwardMessage(msg)
	}
}

func (p *Peer) OID() uint64 {
	return p.ID
}

// NO OP because Peer will not be executing anything locally.
// This is implemented purely to meet the interface requirements
// of Value
//
func (p *Peer) Return(am *AsyncMsg) {}
