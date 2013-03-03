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
	"disco/crypt"
	"disco/file"
	"fmt"
	"os"
	"path"
	"path/filepath"
	"strings"
)

var GetUsage = `Usage:
    disco get <url>
    
    Retrieves a manifest file located at the
    specified URL.
    
    Within a project directory, the file will
    be stored in the project's ".lib" directory.  
    Outside of a project directory, the manifest 
    will be stored in the "~/.disco.root/lib" 
    directory.
    
    If broker keys have not yet been generated 
    for the broker, they will be generated upon
    the execution of this command. 

Example (within the Test project):
    $ disco get https://example.com/1/Nil.dm
        created .disco/brokers/example.com
        written .disco/brokers/example.com/pub.key
        written .disco/brokers/example.com/prv.key
        created user @ https://example.com
        written lib/Nil.dm
`

func Get(args []string) {
	if len(args) < 1 {
		fmt.Println("disco get: missing library url")
		fmt.Println(GetUsage)
		os.Exit(1)
	}

	bdir := brokerDirName(args[0]) 

	pwd, _ := os.Getwd()
	proj := file.ProjDirFrom(pwd)
	if proj == "" {
		createRootBrokerDir(bdir)
	} else {
		p := path.Join(proj, ".disco/brokers")
		createProjBrokerDir(p, bdir)
	}
}

func brokerDirName(url string) string {
	domain := strings.Split(url, "/")
	d := strings.Split(domain[2], ".")

	broker := strings.Join(d, ".")
	dir := strings.Replace(broker, ":", "_", 1)

	return dir
}

func createProjBrokerDir(pdir, bdir string) {
	path := path.Join(pdir, bdir)
	err := os.Mkdir(path, 0700)
	if err != nil {
		displayGetError("error creating broker dir", err)
	}
	fmt.Printf("    created .disco/%s\n", bdir)

	writeKeysTo(path)
}

func createRootBrokerDir(bdir string) {

}

func writeKeysTo(bdir string) {
	pub, priv, kerr := crypt.CreateBrokerKeys()
	if kerr != nil {
		displayGetError("error creating broker keys", kerr)
	}

	public := writeKey("pub.key", pub, bdir)
	if public {
		fmt.Printf("    written .disco/%s/pub.key\n", filepath.Base(bdir))
	}

	private := writeKey("prv.key", priv, bdir)
	if private {
		fmt.Printf("    written .disco/%s/prv.key\n", filepath.Base(bdir))
	}
}

func writeKey(name string, key []byte, dir string) bool {
	path := path.Join(dir, name)
	file, err := os.Create(path)
	defer file.Close()

	if err != nil {
		displayGetError("error creating key file", err)
		return false
	}

	_, werr := file.Write(key)
	if werr != nil {
		displayGetError("error writing key file", werr)
		return false
	}

	return true
}

func displayGetError(msg string, err error) {
	if err == nil {
		fmt.Printf("disco get: %s\n", msg)
	} else {
		fmt.Printf("disco get: %s: %s\n", msg, err)
	}
	os.Exit(1)
}
