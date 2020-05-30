package main

import (
	"flag"
	"fmt"
	"log"
	"os"

	"github.com/szermatt/emacsclient"
)

func main() {
	clientOptions := emacsclient.OptionsFromFlags()
	quoted := true
	file := ""
	flag.BoolVar(&quoted, "q", false, "Shorthand for --quoted")
	flag.BoolVar(&quoted, "quoted", false, "Keep quoted strings.")
	flag.StringVar(&file, "file", "", "File to open afterwards")
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
	if len(file) > 0 {
		emacsclient.SendFile(c, file)
	}
	responses := make(chan emacsclient.Response, 1)
	go emacsclient.Receive(c, responses)
	if quoted {
		err = emacsclient.WriteAll(responses, os.Stdout)
	} else {
		err = emacsclient.WriteUnquoted(responses, os.Stdout)
	}
	if err != nil {
		emacsclient.WriteError(err, os.Stderr)
		os.Exit(1)
	}
}
