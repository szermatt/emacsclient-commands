// +build !windows

// Utilities for building fifo to send and receive
// whole files.
package emacsclient

import (
	"bufio"
	"io/ioutil"
	"os"
	"path"
	"syscall"
)

// Convenience wrapper around a named pipes, stored in a temporary
// file. The temporary file is deleted when Fifo.Close is called.
type Fifo struct {
	Path string
	file *os.File
}

// Creates a named pipe.
func CreateFifo(options *Options) (*Fifo, error) {
	tmpfile, err := ioutil.TempFile(tempDir(options), "fifo.*")
	if err != nil {
		return nil, err
	}
	path := tmpfile.Name()
	tmpfile.Close()
	os.Remove(path)
	if err := syscall.Mkfifo(path, 0600); err != nil {
		return nil, err
	}
	return &Fifo{Path: path, file: nil}, nil
}

// openForWrite, open the fifo, if necessary.
func (o *Fifo) openForWrite() error {
	if o.file != nil {
		return nil
	}
	var err error
	o.file, err = os.OpenFile(o.Path, os.O_WRONLY, 0)
	return err
}

// WriteStdin writes stdin to the FIFO.
func (o *Fifo) WriteStdin() error {
	return o.ChainWrites([]byte{}, bufio.NewReader(os.Stdin))
}

// ChainWrites writes prelude, then the content of readed to the FIFO.
func (o *Fifo) ChainWrites(prelude []byte, reader *bufio.Reader) error {
	if err := o.openForWrite(); err != nil {
		return err
	}
	if len(prelude) > 0 {
		_, err := o.file.Write(prelude)
		if err != nil {
			return err
		}
	}
	_, err := reader.WriteTo(o.file)
	return err
}

// Close closes the FIFO and deletes the file.
func (o *Fifo) Close() {
	if o.file != nil {
		o.file.Close()
	}
	os.Remove(o.Path)
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
