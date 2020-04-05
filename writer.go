// Utilities for sending emacsclient.Response to a writer.
package emacsclient

import (
	"errors"
	"io"
	"os"
)

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

// WriteAll writes the text of all responses to the given output,
// until there's nothing left to read.
func WriteAll(responses chan Response, out *os.File) error {
	first := true
	for {
		response, more := <-responses
		if !more {
			if !first {
				out.WriteString("\n")
			}
			return nil
		}
		switch response.Type {
		case SuccessResponse:
			if !first {
				out.WriteString("\n")
			}
			first = false
			out.WriteString(response.Text)
		case ContinueResponse:
			out.WriteString(response.Text)
		case ErrorResponse:
			return errors.New(response.Text)
		}
	}
}
