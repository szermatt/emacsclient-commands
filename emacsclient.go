// Utilities for communicating with an Emacs server.
package emacsclient

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"net"
	"os"
	"strings"
)

// DefaultSocketName returns the default Emacs server socket for the current user.
func DefaultSocketName() string {
	return fmt.Sprintf("%semacs%d/server", os.TempDir(), os.Getuid())
}

// Eval sends a elisp expression to Emacs to evaluate.
//
// It returns the result as a string.
func SendEval(c net.Conn, elisp string) error {
	_, err := io.WriteString(c, "-eval "+quoteArgument(elisp)+" ")
	return err
}

type closeWriter interface {
	CloseWrite() error
}

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
