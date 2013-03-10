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
	"os/user"
	"path"
	"path/filepath"
	"strings"
)

var GetUsage = `Usage:
    disco get [library url]+
    
    Retrieves a one or more library files located
    at the specified URLs.
    
    Within a project directory, the file will
    be stored in the project's "lib" directory.  
    Outside of a project directory, the library 
    will be stored in the "~/.disco.root/lib" 
    directory.
    
    If broker keys have not yet been generated 
    for the broker, they will be generated upon
    the execution of this command. 

Example (within the Test project):
    $ disco get https://example.com/1/Nil.dm
        created .disco/brokers/example.com
        written .disco/brokers/example.com/peer_pub.key
        written .disco/brokers/example.com/peer_prv.key
	written .disco/brokers/example.com/sign_pub.key
	written .disco/brokers/example.com/sign_prv.key
        created user @ https://example.com
        written lib/Nil.dm
`

func Get(args []string) {
	if len(args) < 1 {
		fmt.Println("disco get: missing library url(s)")
		fmt.Println(GetUsage)
		os.Exit(1)
	}

	pwd, _ := os.Getwd()
	proj := file.ProjDirFrom(pwd)

	var brokerDir string
	var brokerDirFunc func(string, string)
	if proj == "" {
		user, _ := user.Current()
		root := path.Join(user.HomeDir, ".disco.root")

		brokerDir = path.Join(root, "brokers")
		brokerDirFunc = createRootBrokerDir
	} else {
		root := path.Join(proj, ".disco")

		brokerDir = path.Join(root, "brokers")
		brokerDirFunc = createProjBrokerDir
	}

	for _, arg := range args {
		bdir := brokerDirName(arg)
		if dir := path.Join(brokerDir, bdir); file.DirDoesNotExist(dir) {
			brokerDirFunc(brokerDir, bdir)
		}
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
		displayGetError("error creating project broker dir", err)
	}
	fmt.Printf("    created .disco/brokers/%s\n", bdir)

	writeKeysTo(path)
}

func createRootBrokerDir(rdir, bdir string) {
	path := path.Join(rdir, bdir)
	err := os.Mkdir(path, 0700)
	if err != nil {
		displayGetError("error creating root broker dir", err)
	}
	fmt.Printf("    created .disco.root/brokers/%s\n", bdir)

	writeKeysTo(path)
}

func writeKeysTo(bdir string) {
	keyDir := filepath.Base(bdir)
	root := filepath.Dir(filepath.Dir(bdir))

	ppub, pprv, perr := crypt.CreatePeerKeys()
	if perr != nil {
		displayGetError("error creating broker peer keys", perr)
	}

	peerPub := writeKey("peer_pub.key", ppub, bdir)
	if peerPub {
		fmt.Printf("    written %s/brokers/%s/peer_pub.key\n", filepath.Base(root), keyDir)
	}

	peerPrv := writeKey("peer_prv.key", pprv, bdir)
	if peerPrv {
		fmt.Printf("    written %s/brokers/%s/peer_prv.key\n", filepath.Base(root), keyDir)
	}

	spub, sprv, serr := crypt.CreateSignKeys()
	if serr != nil {
		displayGetError("error creating broker sign keys", serr)
	}

	signPub := writeKey("sign_pub.key", spub, bdir)
	if signPub {
		fmt.Printf("    written %s/brokers/%s/sign_pub.key\n", filepath.Base(root), keyDir)
	}

	signPrv := writeKey("sign_prv.key", sprv, bdir)
	if signPrv {
		fmt.Printf("    written %s/brokers/%s/sign_prv.key\n", filepath.Base(root), keyDir)
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
	fmt.Printf("disco get: %s: %s\n", msg, err)
	os.Exit(1)
}
