// +build windows

// Utilities for building fifo to send and receive
// whole files.
package emacsclient

import (
	"bufio"
	"crypto/rand"
	"encoding/hex"
	"io"
	"os"

	fifo "github.com/hashicorp/nomad/client/lib/fifo"
)

func uPipeName(prefix string) string {
	randBytes := make([]byte, 16)
	rand.Read(randBytes)
	return prefix + hex.EncodeToString(randBytes)
}

// Convenience wrapper around a named pipes, stored in a temporary
// file. The temporary file is deleted when Fifo.Close is called.
type Fifo struct {
	Path   string
	reader io.ReadCloser
	writer io.WriteCloser
}

// Creates a named pipe.
func CreateFifo(options *Options) (*Fifo, error) {
	var path string = `\\.\path\` + uPipeName("fifo_")

	readerOpenFn, err := fifo.CreateAndRead(path)
	if err != nil {
		return nil, err
	}

	reader, err := readerOpenFn()
	if err != nil {
		return nil, err
	}

	writer, err := fifo.OpenWriter(path)
	if err != nil {
		return nil, err
	}

	return &Fifo{path, reader, writer}, nil
}

// WriteStdin writes stdin to the FIFO.
func (o *Fifo) WriteStdin() error {
	return o.ChainWrites([]byte{}, bufio.NewReader(os.Stdin))
}

// ChainWrites writes prelude, then the content of readed to the FIFO.
func (o *Fifo) ChainWrites(prelude []byte, reader *bufio.Reader) error {

	if len(prelude) > 0 {
		_, err := o.writer.Write(prelude)
		if err != nil {
			return err
		}
	}
	_, err := reader.WriteTo(o.writer)
	return err
}

// Close closes the FIFO and deletes the file.
func (o *Fifo) Close() {
	o.writer.Close()
	o.reader.Close()
}
