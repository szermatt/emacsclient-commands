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
	if err := emacsclient.ReceiveAndWrite(c, os.Stdout); err != nil {
		os.Stderr.WriteString("*ERROR*: ")
		os.Stderr.WriteString(err.Error())
		os.Stderr.WriteString("\n")
		os.Exit(1)
	}
}
