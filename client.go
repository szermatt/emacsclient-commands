// Utilities for communicating with an Emacs server.
package emacsclient

import (
	"bufio"
	"errors"
	"flag"
	"fmt"
	"io"
	"net"
	"os"
	"path"
	"strings"
)

// #include <unistd.h>
import "C"

// Client dialing options.
type Options struct {
	SocketName string
}

// OptionsFromFlags returns client options controlled by standard command-line flags.
func OptionsFromFlags() *Options {
	options := &Options{}
	flag.StringVar(&options.SocketName, "socket-name", defaultSocketName(), "Emacs server unix socket")
	return options
}

// defaultSocketName returns the default Emacs server socket for the current user.
func defaultSocketName() string {
	fromEnv := os.Getenv("EMACS_SOCKET_NAME")
	if fromEnv != "" {
		return fromEnv
	}
	return path.Join(os.TempDir(), fmt.Sprintf("emacs%d", os.Getuid()), "server")
}

// Dial connects to the remote Emacs server.
func Dial(options *Options) (net.Conn, error) {
	conn, err := net.Dial("unix", options.SocketName)
	if err != nil {
		return nil, err
	}
	if err = initConnection(conn); err != nil {
		conn.Close()
		return nil, err
	}
	return conn, nil
}

// initConnection initializes the connection with Emacs.
func initConnection(c net.Conn) error {
	return sendPWD(c)
}

// sendPWD sends the current directory to Emacs.
func sendPWD(c net.Conn) error {
	pwd := os.Getenv("PWD")
	if pwd == "" {
		cwd, err := os.Getwd()
		if err != nil {
			return err
		}
		pwd = cwd
	}
	_, err := io.WriteString(c, "-dir "+quoteArgument(pwd)+"/ ")
	return err
}

// SendTTY sends the current terminal information to Emacs.
func SendTTY(c net.Conn) error {
	ttyType := os.Getenv("TERM")
	cTtyName := C.ttyname(1)
	if cTtyName == nil {
		return errors.New("No TTY")
	}
	ttyName := C.GoString(cTtyName)
	_, err := io.WriteString(c, "-tty "+quoteArgument(ttyName)+" "+quoteArgument(ttyType)+" ")
	return err
}

// SendCreateFrame tells Emacs to create a new frame.
func SendCreateFrame(c net.Conn) error {
	_, err := io.WriteString(c, "-window-system ")
	return err
}

// SendEval sends a elisp expression to Emacs to evaluate.
//
// It returns the result as a string.
func SendEval(c net.Conn, elisp string) error {
	_, err := io.WriteString(c, "-eval "+quoteArgument(elisp)+" ")
	return err
}

// SendEvalFromTemplate calls SendEval on the result of ExecuteTemplate.
func SendEvalFromTemplate(c net.Conn, args interface{}, template string) error {
	expr, err := ExecuteTemplate(args, template)
	if err != nil {
		return err
	}
	return SendEval(c, expr)
}

type closeWriter interface {
	CloseWrite() error
}

// sendDone tells Emacs we're done sending commands.
func sendDone(c net.Conn) error {
	if _, err := io.WriteString(c, "\n"); err != nil {
		return err
	}
	return nil
}

// Response received from the Emacs server
type Response struct {
	Type ResponseType
	Text string
}

type ResponseType int32

const (
	SuccessResponse  ResponseType = 0
	ContinueResponse ResponseType = 1
	ErrorResponse    ResponseType = 2
)

// Receive closes the write channel and reads responses from Emacs,
// puts them into responses and closes the channel.
func Receive(c net.Conn, responses chan Response) error {
	if err := sendDone(c); err != nil {
		return err
	}

	input := bufio.NewScanner(c)
	for input.Scan() {
		line := input.Text()
		switch {
		case strings.HasPrefix(line, "-print "):
			responses <- Response{
				Type: SuccessResponse,
				Text: unquoteArgument(line[len("-print "):]),
			}
		case strings.HasPrefix(line, "-print-nonl "):
			responses <- Response{
				Type: ContinueResponse,
				Text: unquoteArgument(line[len("-print-nonl "):]),
			}
		case strings.HasPrefix(line, "-error "):
			responses <- Response{
				Type: ErrorResponse,
				Text: unquoteArgument(line[len("-error "):]),
			}
		}
	}
	close(responses)
	return input.Err()
}

// quoteArgument quotes the given string to send to the Emacs server.
func quoteArgument(unquoted string) string {
	var quoted strings.Builder
	runes := []rune(unquoted)
	for len(runes) > 0 && runes[0] == '-' {
		quoted.WriteString("&-")
		runes = runes[1:]
	}
	for _, c := range runes {
		switch c {
		case ' ':
			quoted.WriteString("&_")
		case '\n':
			quoted.WriteString("&n")
		case '&':
			quoted.WriteString("&&")
		default:
			quoted.WriteRune(c)
		}
	}
	return quoted.String()
}

// appendUnquoted unquotes a string received from the Emacs server.
// It writes the result to the given string builder.
func unquoteArgument(quoted string) string {
	var unquoted strings.Builder
	amp := false
	for _, r := range quoted {
		if amp {
			switch r {
			case '_':
				unquoted.WriteRune(' ')
			case 'n':
				unquoted.WriteRune('\n')
			default:
				unquoted.WriteRune(r)
			}
			amp = false
		} else if r == '&' {
			amp = true
		} else {
			unquoted.WriteRune(r)
		}
	}
	return unquoted.String()
}
