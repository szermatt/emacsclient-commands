// Utilities for building fifo to send and receive
// whole files.
package emacsclient

import (
	"bufio"
	"github.com/hashicorp/nomad/client/lib/fifo"
	"io/ioutil"
	"os"
	"path"
	"syscall"
)

// from: src/os/tempfile.go
func fastrand() uint32

func nextRandom() string {
	return uitoa(uint(fastrand()))
}

// uPipeName creates a unique fifo pipe name
func uPipeName() string {
	return "fifo" + nextRandom()
}

// Convenience wrapper around a named pipes, stored in a temporary
// file. The temporary file is deleted when Fifo.Close is called.
type Fifo struct {
	Path string
	pipe *fifo.winFIFO
}

// Creates a named pipe.
func CreateFifo(options *Options) (*Fifo, error) {
	var path string = `//./path/` + uPipeName()

	readerOpenFn, err := fifo.CreateAndRead(path)
	if err != nil {
		return nil, err
	}

	return &Fifo{path, readerOpenFn()}, nil
}

// WriteStdin writes stdin to the FIFO.
func (o *Fifo) WriteStdin() error {
	return o.ChainWrites([]byte{}, bufio.NewReader(os.Stdin))
}

// ChainWrites writes prelude, then the content of readed to the FIFO.
func (o *Fifo) ChainWrites(prelude []byte, reader *bufio.Reader) error {
	// writer, err := OpenWriter(o.Path)
	// if err != nil {
	// 	return err
	// }
	if len(prelude) > 0 {
		_, err := o.pipe.Write(prelude)
		if err != nil {
			return err
		}
	}
	_, err := reader.WriteTo(o.pipe)
	return err
}

// Close closes the FIFO and deletes the file.
func (o *Fifo) Close() {
	return o.pipe.Close()
}

// Returns a temporary directory suitable for storing the temporary
// fifo file.
//
// Emacs puts the socket in a user-owned directory. Put the temporary
// fifo file there when possible.
//
// Returns "" to use the system's temporary directory.
func tempDir(options *Options) string {
	var dir string

	if checkPath(options.SocketName) {
		dir = path.Dir(options.SocketName)
	} else if checkPath(options.ServerFile) {
		dir = path.Dir(options.ServerFile)
	}

	if dir == "." {
		return ""
	}
	stat, err := os.Stat(dir)
	if err == nil && stat.IsDir() && isOwned(stat) && stat.Mode()&0600 == 0600 {
		return dir
	}
	return ""
}

// isOwned strictly compares file owner with the current user's UID on
// Posix systems.
func isOwned(stat os.FileInfo) bool {
	systat := stat.Sys().(*syscall.Stat_t)
	return systat != nil && systat.Uid == uint32(os.Getuid())
}
