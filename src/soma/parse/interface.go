package parse

import (
	"io/ioutil"
	"os"
	"path/filepath"
	"soma/ast"
	"soma/file"
	"soma/rt"
)

func ParseDir(fset *file.FileSet, path string, filter func(os.FileInfo) bool) (files map[string]*ast.File, first error) {
	fd, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer fd.Close()

	list, err := fd.Readdir(-1)
	if err != nil {
		return nil, err
	}

	files = make(map[string]*ast.File)
	for _, fi := range list {
		if filter == nil || filter(fi) {
			filename := filepath.Join(path, fi.Name())
			if src, err := ParseFile(fset, filename, nil); err == nil {
				files[filename] = src
			} else if first == nil {
				first = err
			}
		}
	}

	return
}

func ParseExpr(src string) ([]rt.Expr, error) {
	fset := file.NewFileSet()
	file, err := ParseFile(fset, "", src)

	return file.Exprs, err
}

func ParseFile(fset *file.FileSet, filename string, src interface{}) (*ast.File, error) {
	text, err := readSource(filename, src)

	if err != nil {
		return nil, err
	}

	file := fset.AddFile(filename, fset.Base(), len(text))

	var p Parser
	p.Init(file, text)
	f := p.parseFile()

	return f, p.errors.Err()
}

func readSource(filename string, src interface{}) ([]byte, error) {
	if src != nil {
		switch s := src.(type) {
		case string:
			return []byte(s), nil
		}
	}
	return ioutil.ReadFile(filename)
}
