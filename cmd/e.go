package main

import (
	"flag"
	"fmt"
	"log"
	"os"

	"github.com/szermatt/emacsclient"
)

var (
	clientOptions  = emacsclient.OptionsFromFlags()
	unquoteStrings = flag.Bool("unquote", true, "Remove quotes from elisp strings.")
)

func main() {
	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "e evaluates a lisp expression and prints the result.\n")
		fmt.Fprintf(os.Stderr, "usage: e {args} expression {expressions...}\n")
		flag.PrintDefaults()
	}
	flag.Parse()

	if len(flag.Args()) == 0 {
		fmt.Fprintf(os.Stderr, "ERROR: missing expression\n")
		flag.Usage()
		os.Exit(3)
	}

	c, err := emacsclient.Dial(clientOptions)
	if err != nil {
		log.Fatal(err)
	}
	defer c.Close()

	for _, arg := range flag.Args() {
		if err := emacsclient.SendEval(c, arg); err != nil {
			log.Fatal(err)
		}
	}
	if err := emacsclient.SendDone(c); err != nil {
		log.Fatal(err)
	}
	responses := make(chan emacsclient.Response, 1)
	go emacsclient.Receive(c, responses)
	if *unquoteStrings {
		err = emacsclient.WriteUnquoted(responses, os.Stdout)
	} else {
		err = emacsclient.WriteAll(responses, os.Stdout)
	}
	if err != nil {
		emacsclient.WriteError(err, os.Stderr)
		os.Exit(1)
	}
}
