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
	"bytes"
	"disco/file"
	"disco/rt"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"path/filepath"
	"strings"
)

var ServeUsage = `Usage:
disco serve <broker url>

Serves a project to the specified broker URL on port
10810.

Example (within the project 'Test'):
    $ disco serve https://example.com
        serving Test => https://example.com on 10810
`

func Serve(args []string) {
	if len(args) < 1 {
		displayServeError("missing broker url", nil)
	}

	pwd, _ := os.Getwd()
	pd := file.ProjDirFrom(pwd)
	if pd == "" {
		displayServeError("must be called within discourse project", nil)
	}

	scope := rt.NewScope(nil)
	_, err := LoadProjectDir(pd, scope)
	if err != nil {
		displayServeError("failed to load project directory", err)
	}

	pname := filepath.Base(pd)
	body := encodeProject(pname)
	postErr := postProjectToBroker(args[0], pname, body)
	if postErr != nil {
		displayServeError("failed to post project to broker", err)
	}

	ln, port := rt.StartListening(10810)
	rt.RT.Port = port

	log.Printf("Serving '%s' on %d => %s\n", pname, port, args[0])
	log.Fatal(http.Serve(ln, nil))
}

type jsonProject struct {
	Name    string
	Port    int
	RID     uint64
	Objects []jsonObject
}

type jsonObject struct {
	Name      string
	OID       uint64
	Behaviors map[string]uint64
}

func encodeProject(pname string) *bytes.Buffer {
	project := &jsonProject{pname, 10810, rt.RT.ID, []jsonObject{}}

	values, heap := rt.RT.Globals.Values, rt.RT.Heap
	for index, name := range rt.RT.Globals.Order {
		oid := values[index]
		value := heap.Lookup(oid)
		behaviors := value.(*rt.Object).Behaviors

		obj := jsonObject{name, oid, behaviors}
		project.Objects = append(project.Objects, obj)
	}

	json, err := json.Marshal(project)
	if err != nil {
		displayServeError("error encoding project", err)
	}

	return bytes.NewBuffer(json)
}

func postProjectToBroker(url string, pname string, body io.Reader) error {
	projectURL := strings.Join([]string{url, "p", pname}, "/")
	resp, err := http.Post(projectURL, "text/json", body)
	if err != nil {
		return err
	}

	switch resp.StatusCode {
	case 201:
		return nil
	case 409:
		displayServeError("project name already in use at broker", nil)
	default:
		displayServeError("the broker was unable to receive the project ("+resp.Status+")", nil)
	}

	return nil
}

func displayServeError(msg string, err error) {
	if err != nil {
		fmt.Printf("disco serve: %s: %s\n", msg, err)
	} else {
		fmt.Printf("disco serve: %s\n", msg)
	}

	os.Exit(1)
}
