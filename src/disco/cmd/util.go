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
	"disco/file"
	"disco/parse"
	"disco/rt"
	"encoding/json"
	"io/ioutil"
	"net/http"
	"os"
	"path"
	"path/filepath"
)

func LoadProjectDir(pd string, scope *rt.Scope) (*rt.Scope, error) {
	_, err := loadProjectsFromManifest(pd)
	if err != nil {
		return nil, err
	}

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
// a warning)
//
func loadProjectsFromManifest(pd string) ([]*project, error) {
	lib := pd + "/lib/manifest.dm"
	bytes, err := ioutil.ReadFile(lib)
	if err != nil {
		return []*project{}, nil
	}

	var m manifest
	jerr := json.Unmarshal(bytes, &m)
	if jerr != nil {
		return []*project{}, nil
	}

	projects := make([]*project, len(m.Libs))
	for _, url := range m.Libs {
		resp, gerr := http.Get(url)
		if gerr != nil {
			return nil, err
		}
	
		body, rerr := ioutil.ReadAll(resp.Body)
		if rerr != nil {
			return nil, rerr
		}
		resp.Body.Close()

		var p *project
		perr := json.Unmarshal(body, &p)
		if perr == nil {
			projects = append(projects, p)
		}
	}

	return projects, nil
}

func isLangFile(info os.FileInfo) bool {
	match, _ := path.Match("*.disco", info.Name())
	return match
}
