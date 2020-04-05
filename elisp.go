// Utilities for generating and parsing elisp types in
// text form.
package emacsclient

import (
	"errors"
	"fmt"
	"io"
	"strings"
)

//
type ParseError struct {
	Response    Response
	Description string
}

func (e *ParseError) Error() string {
	return fmt.Sprintf("%s in <<%s>>", e.Description, e.Response.Text)
}

// ReadElispString reads a single response from the channel and
// interprets it as a boolean.
func ReadBool(responses chan Response) (bool, error) {
	r, more := <-responses
	if !more {
		return false, io.EOF
	}
	switch r.Type {
	case ErrorResponse:
		return false, errors.New(r.Text)

	case SuccessResponse:
		// Emacs transforms empty lists to nil, no everything but nil
		// should be considered true.
		return r.Text != "nil", nil

	default:
		return false, fmt.Errorf("Unexpected response type %v", r)
	}
}

// ReadElispString reads a single string from the given channel.
//
// A string can be large and spread over multiple responses.
//
// Returns a ParseError if what is read isn't a string.
func ReadElispString(responses chan Response, out io.StringWriter) error {
	backslash := false
	for {
		r, more := <-responses
		if !more {
			return io.EOF
		}
		text := r.Text
		switch r.Type {
		case ErrorResponse:
			return errors.New(text)

		case SuccessResponse:
			if !strings.HasPrefix(text, "\"") {
				return &ParseError{r, "Expected an elisp string"}
			}
			text = text[1:]
		case ContinueResponse:
			// Continue processing text
		default:
			return fmt.Errorf("Unexpected response type %v", r)
		}
		for _, r := range text {
			if backslash {
				switch r {
				case '"':
					out.WriteString("\"")
				case 'a':
					out.WriteString("\a")
				case 'b':
					out.WriteString("\b")
				case 't':
					out.WriteString("\t")
				case 'n':
					out.WriteString("\n")
				case 'v':
					out.WriteString("\v")
				case 'f':
					out.WriteString("\f")
				case 'r':
					out.WriteString("\r")
				case 'e':
					out.WriteString(string(27))
				case 's':
					out.WriteString(string(32))
				case 'd':
					out.WriteString(string(127))
				default:
					out.WriteString(string(r))
				}
				backslash = false
				continue
			}
			if r == '\\' {
				backslash = true
				continue
			}
			if r == '"' {
				return nil
			}
			out.WriteString(string(r))
		}
	}
}

func WriteUnquoted(responses chan Response, out io.StringWriter) error {
	first := true
	for {
		if !first {
			out.WriteString("\n")
		}
		first = false
		err := ReadElispString(responses, out)
		if err == nil {
			continue
		}
		if err == io.EOF {
			return nil
		}
		if perr, ok := err.(*ParseError); ok {
			out.WriteString(perr.Response.Text)
			continue
		}
		return err
	}
}
