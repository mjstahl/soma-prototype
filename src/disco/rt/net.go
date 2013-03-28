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
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"log"
	"net"
	"net/http"
	"strings"
)

func LocalIP() (net.IP, error) {
	tt, err := net.Interfaces()
	if err != nil {
		return nil, err
	}

	for _, t := range tt {
		aa, err := t.Addrs()
		if err != nil {
			return nil, err
		}
		for _, a := range aa {
			ipnet, ok := a.(*net.IPNet)
			if !ok {
				continue
			}
			v4 := ipnet.IP.To4()
			if v4 == nil || v4[0] == 127 { // loopback address
				continue
			}
			return v4, nil
		}
	}

	return nil, errors.New("cannot find local IP address")
}

func StartListening(port int) (net.Listener, int) {
	http.HandleFunc("/msg", handleMsgReceived)

	var ln net.Listener
	var err error
	for {
		portStr := fmt.Sprintf(":%d", port)
		ln, err = net.Listen("tcp", portStr)
		if err != nil {
			port = port + 1
		} else {
			break
		}
	}

	return ln, port
}

func handleMsgReceived(w http.ResponseWriter, r *http.Request) {
	switch r.Method {
	case "POST":
		body, _ := ioutil.ReadAll(r.Body)
		defer r.Body.Close()

		w.WriteHeader(http.StatusAccepted)

		var msg RemoteMsg
		json.Unmarshal(body, &msg)
		ip := strings.Split(r.RemoteAddr, ":")[0]

		log.Printf("RECV MSG (%s): %s", msg.Msg.Behavior, r.URL)

		ipAddr := net.ParseIP(ip)
		processRemoteMessage(ipAddr, msg)
	default:
		http.Error(w, "Method Not Allowed", 405)
	}
}

func processRemoteMessage(ip net.IP, msg RemoteMsg) {
	p := RT.Peers[msg.RuntimeID]
	if p == nil {
		p = CreatePeer(ip, msg.Port, msg.RuntimeID)
		RT.Peers[msg.RuntimeID] = p
		go p.New()
	}

	// Here we are looking up either the object or the behavior
	// if Arg[1] is NOT 0 then we are looking up a behavior and 
	// the object is local to the sender.
	var obj Value
	if msg.Msg.Args[1] != 0 {
		obj = RT.Heap.Lookup(msg.Msg.Args[1])
		obj.Address() <- msg.Msg
		return
	}

	// if Args[1] is 0 then we are looking up the object so that
	// the message can be redirected to its behavior
	obj = RT.Heap.Lookup(msg.Msg.Args[0])
	if msg.Msg.PromisedTo != 0 {
		promise := sendAsyncMessage(obj.Address(), msg.Msg.Behavior, msg.Msg.Args)
		promise.Return(msg.Msg)
	} else {
		obj.Address() <- msg.Msg
	}
}
