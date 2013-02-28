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

package file

import (
	"fmt"
	"os"
	"path"
)

func CreateProject(name string, pwd string) {
	projDir := path.Join(pwd, name)
	createProjDir(projDir)
	fmt.Printf("    created %s/\n", name)

	projFileName := path.Join(pwd, name, ".disco")
	createProjFile(projFileName)
	fmt.Printf("    created %s/%s\n", name, ".disco")

	projDocDir := path.Join(projDir, "doc")
	createDocDir(projDocDir)
	fmt.Printf("    created %s/%s\n", name, "doc")

	projLibDir := path.Join(projDir, "lib")
	createLibDir(projLibDir)
	fmt.Printf("    created %s/%s\n", name, "lib")

	projSrcDir := path.Join(projDir, "src")
	createSrcDir(projSrcDir)
	fmt.Printf("    created %s/%s\n", name, "src")

	projMainFile := path.Join(projSrcDir, name+".disco")
	createProjMainFile(projMainFile)
	fmt.Printf("    created %s/%s/%s\n", name, "src", name+".disco")
}

func createProjDir(dir string) {
	err := os.Mkdir(dir, 0700)
	if err != nil {
		fmt.Printf("disco create: error creating project directory: %s\n", err)
		os.Exit(0)
	}
}

func createProjFile(name string) {
	file, err := os.OpenFile(name, os.O_CREATE, 0700)

	defer file.Close()

	if err != nil {
		fmt.Printf("disco create: error creating project root: %s\n", err)
		os.Exit(0)
	}
}

func createLibDir(dir string) {
	err := os.Mkdir(dir, 0700)
	if err != nil {
		fmt.Printf("disco create: error creating project lib directory: %s\n", err)
		os.Exit(0)
	}
}

func createDocDir(dir string) {
	err := os.Mkdir(dir, 0700)
	if err != nil {
		fmt.Printf("disco create: error creating project doc directory: %s\n", err)
	}
}

func createSrcDir(dir string) {
	err := os.Mkdir(dir, 0700)
	if err != nil {
		fmt.Printf("disco create: error creating project src directory: %s\n", err)
		os.Exit(0)
	}
}

func createProjMainFile(file string) {
	mainFile, err := os.OpenFile(file, os.O_CREATE, 0700)

	defer mainFile.Close()

	if err != nil {
		fmt.Printf("error creating main project file: %s\n", err)
		os.Exit(0)
	}
}
