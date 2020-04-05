package main

import (
	"flag"
	"fmt"
	"log"
	"os"

	"github.com/szermatt/emacsclient"
)

var (
	clientOptions = emacsclient.OptionsFromFlags()
)

func main() {
	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "ereg write an Emacs register to stdout.\n")
		fmt.Fprintf(os.Stderr, "usage: ereg {args} [register]\n")
		flag.PrintDefaults()
		fmt.Fprintf(os.Stderr, "  register\n")
		fmt.Fprintf(os.Stderr, "    \tRegister name (defaults to the kill ring)\n")
	}
	flag.Parse()

	var register string
	switch len(flag.Args()) {
	case 0:
		register = ""
	case 1:
		register = flag.Args()[0]
	default:
		fmt.Fprintf(os.Stderr, "ERROR: too many buffer names\n")
		flag.Usage()
		os.Exit(3)
	}

	c, err := emacsclient.Dial(clientOptions)
	if err != nil {
		log.Fatal(err)
	}
	defer c.Close()

	type templateArgs struct {
		Append   bool
		Register string
	}
	if err := emacsclient.SendEvalFromTemplate(
		c, &templateArgs{
			Register: register,
		},
		`(with-temp-buffer
           {{if not .Register}}
             (yank)
           {{else if eq (len .Register) 1}}
             (insert-register {{char .Register}})
           {{else}}
             (insert-register {{str .Register}})
           {{end}}
           (buffer-substring-no-properties (point-min) (point-max)))`); err != nil {
		log.Fatal(err)
	}
	responses := make(chan emacsclient.Response, 1)
	go emacsclient.Receive(c, responses)
	err = emacsclient.WriteUnquoted(responses, os.Stdout)
	if err != nil {
		emacsclient.WriteError(err, os.Stderr)
		os.Exit(1)
	}
}
