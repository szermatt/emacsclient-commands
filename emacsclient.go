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
	"strings"
)

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
	return fmt.Sprintf("%semacs%d/server", os.TempDir(), os.Getuid())
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

// SendEval sends a elisp expression to Emacs to evaluate.
//
// It returns the result as a string.
func SendEval(c net.Conn, elisp string) error {
	_, err := io.WriteString(c, "-eval "+quoteArgument(elisp)+" ")
	return err
}

type closeWriter interface {
	CloseWrite() error
}

// SendDone tells Emacs we're done sending commands.
func SendDone(c net.Conn) error {
	if _, err := io.WriteString(c, "\n"); err != nil {
		return err
	}
	return c.(closeWriter).CloseWrite()
}

func ReceiveAndWrite(c net.Conn, out *os.File) error {
	input := bufio.NewScanner(c)
	first := true
	for input.Scan() {
		line := input.Text()
		switch {
		case strings.HasPrefix(line, "-print "):
			if !first {
				out.WriteString("\n")
			}
			first = false
			out.WriteString(unquoteArgument(line[len("-print "):]))
		case strings.HasPrefix(line, "-print-nonl "):
			first = false
			out.WriteString(unquoteArgument(line[len("-print-nonnl "):]))
		case strings.HasPrefix(line, "-error "):
			if !first {
				out.WriteString("\n")
			}
			return errors.New(unquoteArgument(line[len("-error "):]))
		default:
			continue
		}
	}
	if !first {
		out.WriteString("\n")
	}
	return nil
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
