package main

import (
	"flag"
	"fmt"
	"log"
	"os"
	"strings"

	"github.com/szermatt/emacsclient"
	"gopkg.in/alessio/shellescape.v1"
)

func main() {
	clientOptions := emacsclient.OptionsFromFlags()
	comint := false
	flag.BoolVar(&comint, "c", false, "Shorthand for --comint")
	flag.BoolVar(&comint, "comint", false, "Run compile in comint buffer")
	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "ecompiles calls (compile) on Emacs.\n")
		fmt.Fprintf(os.Stderr, "usage: ecompile {args} compile-command...\n")
		flag.PrintDefaults()
	}
	flag.Parse()

	if len(flag.Args()) == 0 {
		fmt.Fprintf(os.Stderr, "ERROR: compile command missing\n")
		flag.Usage()
		os.Exit(3)
	}

	c, err := emacsclient.Dial(clientOptions)
	if err != nil {
		log.Fatal(err)
	}
	defer c.Close()

	type templateArgs struct {
		Command string
		Env     []string
		Comint  bool
	}
	if err := emacsclient.SendEvalFromTemplate(
		c, &templateArgs{
			Command: buildShellCommand(flag.Args()),
			Env:     os.Environ(),
			Comint:  comint,
		},
		`(let ((compilation-environment '({{strList .Env}})))
           (compile {{str .Command}} {{bool .Comint}}))`); err != nil {
		log.Fatal(err)
	}
	responses := make(chan emacsclient.Response, 1)
	go emacsclient.Receive(c, responses)
	err = emacsclient.ConsumeAll(responses)
	if err != nil {
		emacsclient.WriteError(err, os.Stderr)
		os.Exit(1)
	}
}

func buildShellCommand(unquoted []string) string {
	var quoted []string
	for _, e := range unquoted {
		quoted = append(quoted, shellescape.Quote(e))
	}
	return strings.Join(quoted, " ")
}
