// Utilities for generating and parsing elisp types in text form.
package emacsclient

import (
	"errors"
	"fmt"
	"io"
	"strings"
	"text/template"
)

//
type ParseError struct {
	Response    Response
	Description string
}

func (e *ParseError) Error() string {
	return fmt.Sprintf("%s in <<%s>>", e.Description, e.Response.Text)
}

// ReadBool reads a single response from the channel and
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

// ReadString reads a single string from the given channel.
//
// A string can be large and spread over multiple responses.
//
// Returns a ParseError if what is read isn't a string.
func ReadString(responses chan Response, out io.StringWriter) error {
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

// AsString returns the string representation of an  string.
func AsString(input string) string {
	var out strings.Builder
	out.WriteRune('"')
	for _, r := range input {
		switch r {
		case '\n':
			out.WriteString("\\n")
		case '"':
			out.WriteString("\\\"")
		default:
			out.WriteRune(r)
		}
	}
	out.WriteRune('"')
	return out.String()
}

// AsString returns the string representation of an list of strings.
func AsStringList(input []string) string {
	var out strings.Builder
	out.WriteString("'(")
	first := true
	for _, s := range input {
		if first {
			first = false
		} else {
			out.WriteRune(' ')
		}
		out.WriteString(AsString(s))
	}
	out.WriteRune(')')
	return out.String()
}

// AsBool returns the string representation of an  bool.
func AsBool(val bool) string {
	if val {
		return "t"
	}
	return "nil"
}

// Builds an elisp expression from a template.
//
// The following functions are available in the template:
//  str      - builds a quoted elisp string from a string
//  strList  - builds a quoted elisp string from a []string
//  bool     - transforms a bool into t or nil
func ExecuteTemplate(args interface{}, definition string) (string, error) {
	t := template.New("elisp")
	funcMap := make(template.FuncMap)
	funcMap["str"] = AsString
	funcMap["strList"] = AsStringList
	funcMap["bool"] = AsBool
	t.Funcs(funcMap)
	t, err := t.Parse(definition)
	if err != nil {
		return "", err
	}
	var out strings.Builder
	err = t.Execute(&out, args)
	return out.String(), err
}
