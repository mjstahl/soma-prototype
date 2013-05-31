package file

import (
	"fmt"
	"os"
	"path"
	"path/filepath"
)

func CreateProjectDir(name string, pwd string) {
	fs := createProjectFS(pwd, name)

	createProjectDir(fs, name, "lib")
	mfile := fmt.Sprintf("%s/%s.sm", "lib", "manifest")
	createProjectFile(fs, name, mfile)

	createProjectDir(fs, name, "src")
	pfile := fmt.Sprintf("%s/%s.soma", "src", name)
	createProjectFile(fs, name, pfile)
}

// Given the current directory as an argument, ProjDirFrom will
// go up the directory looking for a social machines project (a
// directory containing a .soma file) once it is found it will
// return the directory.
//
func ProjDirFrom(pwd string) string {
	err := os.Chdir(pwd)
	if err != nil {
		return ""
	}

	if pwd == "." || pwd == "/" {
		return ""
	}

	files, _ := filepath.Glob(".soma")
	if files != nil {
		return pwd
	}

	return ProjDirFrom(filepath.Dir(pwd))
}

func createProjectFS(pwd string, name string) (dir string) {
	dir = path.Join(pwd, name)

	err := os.Mkdir(dir, 0700)
	if err != nil {
		fmt.Printf("soma create: error creating project directory: %s\n", err)
		os.Exit(0)
	}

	fmt.Printf("    created %s/\n", name)
	createProjectDir(dir, name, ".soma")

	return
}

func createProjectDir(pdir string, pname string, dname string) {
	dir := path.Join(pdir, dname)
	err := os.Mkdir(dir, 0700)

	if err != nil {
		fmt.Printf("soma create: error creating project directory '%s': %s\n", dname, err)
		os.Exit(0)
	}

	fmt.Printf("    created %s/%s\n", pname, dname)
}

func createProjectFile(pdir string, pname string, fname string) {
	f := path.Join(pdir, fname)
	file, err := os.OpenFile(f, os.O_CREATE, 0700)

	defer file.Close()

	if err != nil {
		fmt.Printf("soma create: error creating project file '%s': %s\n", fname, err)
		os.Exit(0)
	}

	fmt.Printf("    created %s/%s\n", pname, fname)
}
