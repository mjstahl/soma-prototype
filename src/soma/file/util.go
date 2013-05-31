package file

import (
	"os"
)

func DirDoesNotExist(dir string) bool {
	if _, err := os.Stat(dir); err != nil {
		if os.IsNotExist(err) {
			return true
		}
	}

	return false
}
