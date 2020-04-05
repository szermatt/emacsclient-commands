package main

import (
	"flag"
	"fmt"
	"log"
	"net"
	"os"

	"github.com/szermatt/emacsclient"
)

var (
	socket = flag.String("socket-name", emacsclient.DefaultSocketName(), "Emacs server unix socket")
)

func main() {
	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "e evaluates a lisp expression and prints the result.\n")
		fmt.Fprintf(os.Stderr, "usage: e {args} elisp-expression\n")
		flag.PrintDefaults()
	}
	flag.Parse()

	c, err := net.Dial("unix", *socket)
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
