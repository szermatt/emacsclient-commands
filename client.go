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
	"runtime"
	"strings"
)

// #include <unistd.h>
import "C"

// Client dialing options.
type Options struct {
	SocketName string
	ServerFile string
}

// OptionsFromFlags returns client options controlled by standard command-line flags.
func OptionsFromFlags() *Options {
	options := &Options{}
	flag.StringVar(&options.SocketName, "socket-name", defaultSocketName(), "Emacs server unix socket")
	flag.StringVar(&options.ServerFile, "server-file", defaultServerFile(), "Emacs server TCP file")
	return options
}

// checkPath returns `true` if the folder exists
func checkPath(path string) bool {
	if _, err := os.Stat(path); err != nil {
		if os.IsNotExist(err) {
			return false
		}
	}
	return true
}

// defaultSocketName returns the default Emacs server socket for the current user.
func defaultSocketName() string {
	fromEnv := os.Getenv("EMACS_SOCKET_NAME")
	if checkPath(fromEnv) {
		return fromEnv
	}
	return path.Join(os.TempDir(), fmt.Sprintf("emacs%d", os.Getuid()), "server")
}

// defaultEmacsDir returns the default Emacs configuration directory (aka `.emacs.d`) for the current user.
func defaultEmacsDir() string {
	var emacsDirName string // default folder name used by emacs
	var emacsDir string

	if runtime.GOOS == "windows" {
		emacsDirName = ".emacs.d" // windows always uses '.emacs.d'
	} else {
		emacsDirName = "emacs"
	}

	xdgPathA := path.Join(os.Getenv("XDG_CONFIG_HOME"), emacsDirName)
	xdgPathB := path.Join(os.Getenv("HOME"), ".config", emacsDirName) // user following convention without XDG_CONFIG_HOME set
	legacyPath := path.Join(os.Getenv("HOME"), emacsDirName)          // windows (if HOME is set) and emacs pre-v27

	userConfigDir, _ := os.UserConfigDir()
	osDefaultPath := path.Join(userConfigDir, emacsDirName)

	switch {
	case checkPath(xdgPathA):
		emacsDir = xdgPathA
	case checkPath(xdgPathB):
		emacsDir = xdgPathB
	case checkPath(legacyPath):
		emacsDir = legacyPath
	case checkPath(osDefaultPath):
		emacsDir = osDefaultPath
	}

	return emacsDir
}

// defaultServerFile returns the default Emacs server file for the current user.
func defaultServerFile() (serverFile string) {
	fromEnv := os.Getenv("EMACS_SERVER_FILE")
	fromEmacsDir := path.Join(defaultEmacsDir(), "server", "server")

	if checkPath(fromEnv) {
		serverFile = fromEnv
	} else if checkPath(fromEmacsDir) {
		serverFile = fromEmacsDir
	}

	return
}

// parseServerFile return the emacs server TCP address and auth key from an emacs server file
func parseServerFile(serverFile string) (serverAddr string, authString string) {

	fp, err := os.Open(serverFile)
	if err != nil {
		panic(err)
	}
	defer fp.Close()

	scanner := bufio.NewScanner(fp)
	if scanner.Scan() {
		// 1st line
		serverAddr = strings.Split(scanner.Text(), " ")[0]
	}
	if scanner.Scan() {
		// 2nd line
		authString = scanner.Text()
	}

	return
}

// Dial connects to the remote Emacs server.
func Dial(options *Options) (net.Conn, error) {
	if checkPath(options.SocketName) {
		return dialUnix(options)
	} else {
		return dialTcp(options)
	}
}

// dialUnix connects to an Emacs server instance via Unix Socket.
func dialUnix(options *Options) (net.Conn, error) {
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

// dialTcp connects to an Emacs server instance via TCP.
func dialTcp(options *Options) (net.Conn, error) {
	addr, authKey := parseServerFile(options.ServerFile)
	conn, err := net.Dial("tcp", addr)
	if err != nil {
		return nil, err
	}
	if err = sendAuth(conn, authKey); err != nil {
		conn.Close()
		return nil, err
	}
	return conn, nil

}

// initConnection initializes the connection with Emacs.
func initConnection(c net.Conn) error {
	return sendPWD(c)
}

// sendAuth sends the specified authKey to Emacs.
func sendAuth(c net.Conn, authKey string) error {
	_, err := io.WriteString(c, "-auth "+authKey)
	return err
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
