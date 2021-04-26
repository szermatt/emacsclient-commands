package emacsclient

import (
	"bufio"
	"io/ioutil"
	"os"
	"path"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

func testWrite(t *testing.T, buffer []byte, readerContent string, expected string) {
	fifo, err := CreateFifo(&Options{
		SocketName: "",
		ServerFile: "",
	})
	if err != nil {
		assert.FailNowf(t, "Cannot create fifo: %s", err.Error())
	}
	go func() {
		assert.Nil(t, fifo.ChainWrites(buffer, bufio.NewReader(strings.NewReader(readerContent))))
		fifo.Close()
	}()

	reader, err := os.Open(fifo.Path)
	if err != nil {
		assert.FailNowf(t, "Cannot open fifo at path %s: %s", fifo.Path, err.Error())
	}
	defer reader.Close()

	content, err := ioutil.ReadAll(reader)

	assert.Nil(t, err)
	assert.Equal(t, expected, string(content))
}

func TestWriteToFifoWithPrelude(t *testing.T) {
	testWrite(t, []byte{'h', 'e'}, "llo", "hello")
}

func TestWriteToFifoWithoutPrelude(t *testing.T) {
	testWrite(t, []byte{}, "hello", "hello")
}

func testCreateFifoInSocketDirectory(t *testing.T) {
	dir, err := ioutil.TempDir("", "fifo_test")
	if err != nil {
		assert.FailNowf(t, "Cannot create test directory: %s", err.Error())
	}
	if err = os.Chmod(dir, 0600); err != nil {
		assert.FailNowf(t, "Cannot set directory mode: %s", err.Error())
	}
	fifo, err := CreateFifo(&Options{SocketName: path.Join(dir, "socket")})
	if err != nil {
		assert.FailNowf(t, "Cannot create fifo in dir %s", dir)
	}
	defer fifo.Close()

	assert.Equal(t, dir, path.Dir(fifo.Path))
}
