package emacsclient

import (
	"io"
	"io/ioutil"
	"net"
	"os"
	"path"
	"path/filepath"
	"strings"
	"testing"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
)

func resetEnv(origEnv []string) error {
	for _, pair := range origEnv {
		// Environment variables on Windows can begin with =
		// https://blogs.msdn.com/b/oldnewthing/archive/2010/05/06/10008132.aspx
		i := strings.Index(pair[1:], "=") + 1
		if err := os.Setenv(pair[:i], pair[i+1:]); err != nil {
			return errors.Errorf("Setenv(%q, %q) failed during reset: %v", pair[:i], pair[i+1:], err)
		}
	}
	return nil
}

func TestQuoteArguments(t *testing.T) {
	assert.Equal(t, "hello&_a&&b&n", quoteArgument("hello a&b\n"))
	assert.Equal(t, "&-&-print-this", quoteArgument("--print-this"))
}

func TestUnquoteArguments(t *testing.T) {
	assert.Equal(t, "hello a&b\n", unquoteArgument("hello&_a&&b&n"))
	assert.Equal(t, "--print-this", unquoteArgument("&-&-print&-this"))
}

func TestCheckPath(t *testing.T) {

	t.Run("Check file",
		func(t *testing.T) {
			tmp, _ := ioutil.TempFile("", "checkpath_file")
			assert.True(t, checkPath(tmp.Name()))
			defer os.Remove(tmp.Name())
		})

	t.Run("Check directory",
		func(t *testing.T) {
			tmp, _ := ioutil.TempDir("", "checkpath_dir")
			assert.True(t, checkPath(tmp))
			defer os.Remove(tmp)
		})

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

	// Setup
	defer resetEnv(os.Environ())
	tmp, _ := ioutil.TempFile("", "server-file-test")
	defer os.Remove(tmp.Name())

	os.Setenv("EMACS_SOCKET_NAME", tmp.Name())
	assert.Equal(t, tmp.Name(), defaultSocketName())
}

func TestDefaultSocketName(t *testing.T) {
	assert.Regexp(t, ".*/emacs[0-9]+/server$", defaultSocketName())
}

func TestDefaultServerFile(t *testing.T) {

	// Setup
	tmp, _ := ioutil.TempFile("", "server-file-test")
	defer os.Remove(tmp.Name())

	t.Run("from env",
		func(t *testing.T) {
			defer resetEnv(os.Environ())
			os.Setenv("EMACS_SERVER_FILE", tmp.Name())
			assert.Equal(t, tmp.Name(), defaultServerFile())
		})

	t.Run("from dir",
		func(t *testing.T) {
			fromEmacsDir := path.Join(os.TempDir(), filepath.Base(tmp.Name()))
			assert.Equal(t, fromEmacsDir, defaultServerFile())
		})

}

func TestParseServerFile(t *testing.T) {

	// setup
	var serverFileContents string = `127.0.0.1:62989 17061
;\I^|/+?<egxc[7Qb;6vGCp2:~6nhzcP>:8W#u&*}:@GJj&;ib5KU+).2N}S9Y(e%`

	makeServerFile := func(contents string) string {
		t.Helper()
		tmp, _ := ioutil.TempFile("", "server-file-test")
		tmp.WriteString(contents)
		return tmp.Name()

	}

	serverFile := makeServerFile(serverFileContents)
	defer os.Remove(serverFile)

	t.Run("get address",
		func(t *testing.T) {
			expect := `127.0.0.1:62989`
			got, _, _ := parseServerFile(serverFile)
			assert.Equal(t, expect, got)
		})

	t.Run("get authKey",
		func(t *testing.T) {
			expect := `;\I^|/+?<egxc[7Qb;6vGCp2:~6nhzcP>:8W#u&*}:@GJj&;ib5KU+).2N}S9Y(e%`
			_, got, _ := parseServerFile(serverFile)
			assert.Equal(t, expect, got)
		})

	t.Run("no server file present",
		func(t *testing.T) {
			serverFile := ""
			defer os.Remove(serverFile)
			addr, authKey, got := parseServerFile(serverFile)
			assert.Zero(t, addr)
			assert.Zero(t, authKey)
			assert.Error(t, got)
		})
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
