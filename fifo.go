// Utilities for building fifo to send and receive
// whole files.
package emacsclient

import (
	"bufio"
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
	return WriteToFifo([]byte{}, bufio.NewReader(os.Stdin))
}

// Create a temporary fifo that forwards data from buffer then the
// given reader in the background.
//
// Dies if reading or writing fails.
//
// Caller is responsible for deleting the file.
func WriteToFifo(buffer []byte, reader *bufio.Reader) (string, error) {
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
		if len(buffer) > 0 {
			if _, err := out.Write(buffer); err != nil {
				log.Fatal(err)
			}
		}
		if _, err := reader.WriteTo(out); err != nil {
			log.Fatal(err)
		}
		if err := out.Close(); err != nil {
			log.Fatal(err)
		}
	}()
	return path, nil
}
