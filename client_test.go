package emacsclient

import (
	"github.com/stretchr/testify/assert"
	"io"
	"io/ioutil"
	"net"
	"os"
	"strings"
	"testing"
)

func TestQuoteArguments(t *testing.T) {
	assert.Equal(t, "hello&_a&&b&n", quoteArgument("hello a&b\n"))
	assert.Equal(t, "&-&-print-this", quoteArgument("--print-this"))
}

func TestUnquoteArguments(t *testing.T) {
	assert.Equal(t, "hello a&b\n", unquoteArgument("hello&_a&&b&n"))
	assert.Equal(t, "--print-this", unquoteArgument("&-&-print&-this"))
}

func TestSendPWD(t *testing.T) {
	server, client := net.Pipe()

	go func() {
		oldpwd := os.Getenv("PWD")
		os.Setenv("PWD", "/fakedir")
		assert.Nil(t, sendPWD(client))
		os.Setenv("PWD", oldpwd)
		io.WriteString(client, "\n")
		client.Close()
	}()

	defer server.Close()
	data, err := ioutil.ReadAll(server)
	assert.Nil(t, err)
	assert.Equal(t, "-dir /fakedir/ \n", string(data))
}

func TestDefaultSocketNameFromEnv(t *testing.T) {
	os.Setenv("EMACS_SOCKET_NAME", "/mysocket")
	assert.Equal(t, "/mysocket", defaultSocketName())
	os.Setenv("EMACS_SOCKET_NAME", "")
}

func TestDefaultSocketName(t *testing.T) {
	assert.Regexp(t, ".*/emacs[0-9]+/server$", defaultSocketName())
}

func TestSendEval(t *testing.T) {
	server, client := net.Pipe()

	go func() {
		assert.Nil(t, SendEval(client, "(+ 1 1)"))
		io.WriteString(client, "\n")
		client.Close()
	}()

	defer server.Close()
	data, err := ioutil.ReadAll(server)
	assert.Nil(t, err)
	assert.Equal(t, "-eval (+&_1&_1) \n", string(data))
}

func TestReceive(t *testing.T) {
	server, client := net.Pipe()

	go func() {
		buf := make([]byte, 1)
		_, err := server.Read(buf)
		assert.Nil(t, err)
		assert.Equal(t, []byte{'\n'}, buf)
		io.WriteString(server, "-print \"hell\n")
		io.WriteString(server, "-print-nonl o\"\n")
		io.WriteString(server, "-unknown ignoreme\n")
		io.WriteString(server, "-print t\n")
		io.WriteString(server, "-error fail\n")
		server.Close()
	}()

	c := make(chan Response, 100)
	Receive(client, c)
	client.Close()

	responses := []Response{}
	for {
		response, more := <-c
		if !more {
			break
		}
		responses = append(responses, response)
	}

	assert.Equal(t, []Response{
		Response{
			Type: SuccessResponse,
			Text: "\"hell"},
		Response{
			Type: ContinueResponse,
			Text: "o\""},
		Response{
			Type: SuccessResponse,
			Text: "t"},
		Response{
			Type: ErrorResponse,
			Text: "fail"}},
		responses)
}

func TestReceiveAndRead(t *testing.T) {
	server, client := net.Pipe()

	go func() {
		buf := make([]byte, 1)
		_, err := server.Read(buf)
		assert.Nil(t, err)
		assert.Equal(t, []byte{'\n'}, buf)
		io.WriteString(server, "-print \"hell\n")
		io.WriteString(server, "-print-nonl o\"\n")
		io.WriteString(server, "-unknown ignoreme\n")
		io.WriteString(server, "-print t\n")
		io.WriteString(server, "-error fail\n")
		server.Close()
	}()

	c := make(chan Response, 100)
	go func() {
		Receive(client, c)
		client.Close()
	}()

	str := &strings.Builder{}
	assert.Nil(t, ReadString(c, str))
	assert.Equal(t, "hello", str.String())

	b, err := ReadBool(c)
	assert.Nil(t, err)
	assert.Equal(t, true, b)

	b, err = ReadBool(c)
	assert.Error(t, err)
}
