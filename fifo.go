// Utilities for building fifo to send and receive
// whole files.
package emacsclient

import (
	"bufio"
	"io/ioutil"
	"os"
	"syscall"
)

// Convenience wrapper around a named pipes, stored in a temporary
// file. The temporary file is deleted when Fifo.Close is called.
type Fifo struct {
	Path string
	file *os.File
}

// Creates a named pipe.
func CreateFifo() (*Fifo, error) {
	tmpfile, err := ioutil.TempFile("", "fifo.*")
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
