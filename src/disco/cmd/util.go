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
	"os"
	"os/user"
	"path"
	"path/filepath"
)

func LoadRootDir(scope *rt.Scope) (*rt.Scope, error) {
	user, _ := user.Current()
	path.Join(user.HomeDir, "/.disco.root")

	return scope, nil
}

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

	return scope, nil
}

func isLangFile(info os.FileInfo) bool {
	match, _ := path.Match("*.disco", info.Name())
	return match
}
