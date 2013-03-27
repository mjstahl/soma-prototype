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

package cmd

import (
	"disco/ast"
	"disco/file"
	"disco/parse"
	"disco/rt"
	"encoding/json"
	"io/ioutil"
	"net"
	"net/http"
	"os"
	"path"
	"path/filepath"
)

func LoadProjectDir(pd string, scope *rt.Scope) (*rt.Scope, error) {
	src := pd + "/src"
	files, err := parse.ParseDir(file.NewFileSet(), src, isLangFile)
	if err != nil {
		return nil, err
	}

	pfile := filepath.Base(pd) + ".disco"

	var last string
	for path, file := range files {
		if filepath.Base(path) != pfile {
			file.Visit(scope)
		} else {
			last = path
		}
	}
	files[last].Visit(scope)

	rdefs := libsFromManifest(pd)
	for _, rdef := range rdefs {
		rdef.Visit(scope)
	}

	return scope, nil
}

type manifest struct {
	Libs []string
}

type project struct {
	Name    string
	Peers   []peer
	Objects []object
}

type object struct {
	Name      string
	OID       uint64
	Behaviors map[string]uint64
}

type peer struct {
	Addr string
	Port int
	ID   uint64
}

// Reads the 'lib/manifest.dm' file from the project.  If there 
// is an error reading it then an empty array of projects is 
// returned. If the broker can't be reached or the project is 
// not found an that project is skipped.
//
// TODO(mjs): This behavior is not quite right.  Loading (such 
// as calling 'disco console' should throw up an error, or at least
// a warning).  
//
// TODO(mjs): This function is WAAAY too long. Three different events
// happens here.  This should be split up.  Getting it working first.
//
func libsFromManifest(pd string) []*ast.RDefine {
	lib := pd + "/lib/manifest.dm"
	bytes, err := ioutil.ReadFile(lib)
	if err != nil {
		return []*ast.RDefine{}
	}

	var m manifest
	jerr := json.Unmarshal(bytes, &m)
	if jerr != nil {
		return []*ast.RDefine{}
	}

	projects := []*project{}
	for _, url := range m.Libs {
		resp, gerr := http.Get(url)
		if gerr != nil {
			return nil
		}

		body, rerr := ioutil.ReadAll(resp.Body)
		if rerr != nil {
			return nil
		}
		resp.Body.Close()

		var p *project
		perr := json.Unmarshal(body, &p)
		if perr == nil {
			projects = append(projects, p)
		}
	}

	rdefs := []*ast.RDefine{}
	for _, proj := range projects {
		peers := []*rt.Peer{}

		for _, peer := range proj.Peers {
			ip := net.ParseIP(peer.Addr)
			p := rt.CreatePeer(ip, peer.Port, peer.ID)
			peers = append(peers, p)
		}

		for _, obj := range proj.Objects {
			for behavior, bid := range obj.Behaviors {
				r := &ast.RDefine{obj.Name, obj.OID, behavior, bid, peers}
				rdefs = append(rdefs, r)
			}
		}
	}

	return rdefs
}

func isLangFile(info os.FileInfo) bool {
	match, _ := path.Match("*.disco", info.Name())
	return match
}
