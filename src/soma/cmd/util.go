package cmd

import (
	"encoding/json"
	"io/ioutil"
	"net"
	"net/http"
	"os"
	"path"
	"path/filepath"
	"soma/ast"
	"soma/file"
	"soma/parse"
	"soma/rt"
)

func LoadProjectDir(pd string, scope *rt.Scope) (*rt.Scope, error) {
	src := pd + "/src"
	files, err := parse.ParseDir(file.NewFileSet(), src, isLangFile)
	if err != nil {
		return nil, err
	}

	pfile := filepath.Base(pd) + ".soma"

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

// Reads the 'lib/manifest.sm' file from the project.  If there
// is an error reading it then an empty array of projects is
// returned. If the broker can't be reached or the project is
// not found an that project is skipped.
//
// TODO(mjs): This behavior is not quite right.  Loading (such
// as calling 'soma console' should throw up an error, or at least
// a warning).
//
// TODO(mjs): This function is WAAAY too long. Three different events
// happens here.  This should be split up.  Getting it working first.
//
func libsFromManifest(pd string) []*ast.RemoteObject {
	lib := pd + "/lib/manifest.sm"
	bytes, err := ioutil.ReadFile(lib)
	if err != nil {
		return []*ast.RemoteObject{}
	}

	var m manifest
	jerr := json.Unmarshal(bytes, &m)
	if jerr != nil {
		return []*ast.RemoteObject{}
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

	rdefs := []*ast.RemoteObject{}
	for _, proj := range projects {
		peers := []*rt.Peer{}

		for _, peer := range proj.Peers {
			ip := net.ParseIP(peer.Addr)
			p := rt.CreatePeer(ip, peer.Port, peer.ID)
			peers = append(peers, p)
		}

		for _, obj := range proj.Objects {
			for behavior, bid := range obj.Behaviors {
				r := &ast.RemoteObject{obj.Name, obj.OID, behavior, bid, peers}
				rdefs = append(rdefs, r)
			}
		}
	}

	return rdefs
}

func isLangFile(info os.FileInfo) bool {
	match, _ := path.Match("*.soma", info.Name())
	return match
}
