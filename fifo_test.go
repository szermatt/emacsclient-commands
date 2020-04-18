package emacsclient

import (
	"bufio"
	"github.com/stretchr/testify/assert"
	"io/ioutil"
	"os"
	"strings"
	"testing"
)

func testWrite(t *testing.T, buffer []byte, readerContent string, expected string) {
	fifo, err := CreateFifo()
	if err != nil {
		t.FailNow()
	}
	go func() {
		assert.Nil(t, fifo.ChainWrites(buffer, bufio.NewReader(strings.NewReader(readerContent))))
		fifo.Close()
	}()

	reader, err := os.Open(fifo.Path)
	if err != nil {
		t.FailNow()
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
