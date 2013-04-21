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
	"io/ioutil"
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
	Msg       *AsyncMsg
	Peers     []*ExternalPeer
	Port      int
	RuntimeID uint64
}

type ExternalPeer struct {
	ID   uint64
	IP   string
	Port int
}

func (p *Peer) ForwardMessage(msg Message) {
	ipaddr := fmt.Sprintf("%s:%d", p.IPAddr, p.Port)
	url := fmt.Sprintf("http://%s/msg", ipaddr)

	var peers []*ExternalPeer
	amsg := msg.(*AsyncMsg)
	for _, arg := range amsg.Args {
		rid := (arg >> 36) << 36
		peer := RT.Peers[rid]
		if peer != nil {
			extPeer := &ExternalPeer{peer.ID, peer.IPAddr.String(), peer.Port}
			peers = append(peers, extPeer)
		}
	}
	rmsg := &RemoteMsg{Port: RT.Port, RuntimeID: RT.ID, Msg: amsg, Peers: peers}
	json, _ := json.Marshal(rmsg)
	body := bytes.NewBuffer(json)

	http.Post(url, "text/json", body)
}

func (p *Peer) String() string {
	expr := p.RequestValueExpr()
	if expr == "" {
		id := p.ID & 0xFFFFFFFFF
		return fmt.Sprintf("%s (0x%x @ %s:%d)", "Remote", id, p.IPAddr, p.Port)
	}
	return expr
}

func (p *Peer) RequestValueExpr() string {
	ipaddr := fmt.Sprintf("%s:%d", p.IPAddr, p.Port)
	url := fmt.Sprintf("http://%s/expr/%d", ipaddr, p.ID)

	resp, err := http.Get(url)
	if err != nil {
		return ""
	}
	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return ""
	}
	bytes := bytes.NewBuffer(body)
	return bytes.String()
}

func (p *Peer) LookupBehavior(name string) Value {
	return p
}

func (p *Peer) New() {
	go func() {
		for {
			msg := <-p.Address()
			p.ForwardMessage(msg)
		}
	}()
}

func (p *Peer) OID() uint64 {
	return p.ID
}

// NO OP because Peer will not be executing anything locally.
// This is implemented purely to meet the interface requirements
// of Value
//
func (p *Peer) Return(am *AsyncMsg) {}
