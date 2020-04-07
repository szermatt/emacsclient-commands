// Utilities for building fifo to send and receive
// whole files.
package emacsclient

import (
	"io"
	"io/ioutil"
	"log"
	"os"
)

// Create a temporary fifo that forwards data from stdin in the
// background.
//
// Dies if reading from stdin fails.
//
// Caller is responsible for deleting the file.
func StdinToFifo() (string, error) {
	tmpfile, err := ioutil.TempFile("", "fifo.*")
	if err != nil {
		return "", err
	}
	path := tmpfile.Name()
	tmpfile.Close()
	out, err := os.OpenFile(path, os.O_WRONLY, os.ModeNamedPipe)
	if err != nil {
		return "", err
	}
	go func() {
		if _, err := io.Copy(out, os.Stdin); err != nil {
			log.Fatal(err)
		}
		if err := out.Close(); err != nil {
			log.Fatal(err)
		}
	}()
	return path, nil
}
