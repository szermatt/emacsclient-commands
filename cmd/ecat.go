package main

import (
	"flag"
	"fmt"
	"log"
	"os"

	"github.com/szermatt/emacsclient"
)

var (
	mark          = flag.Bool("mark", false, "Restrict output to the content of the mark")
	clientOptions = emacsclient.OptionsFromFlags()
)

func main() {
	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "ecat outputs the content of an emacs buffer.\n")
		fmt.Fprintf(os.Stderr, "usage: ecompile {args} [buffer-name]\n")
		flag.PrintDefaults()
		fmt.Fprintf(os.Stderr, "  buffer-name\n")
		fmt.Fprintf(os.Stderr, "    \tName of the buffer (default to last active non-shell buffer)\n")
	}
	flag.Parse()

	var bufferName string
	switch len(flag.Args()) {
	case 0:
		bufferName = ""
	case 1:
		bufferName = flag.Args()[0]
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
		BufferName string
		Mark       bool
	}
	if err := emacsclient.SendEvalFromTemplate(
		c, &templateArgs{
			BufferName: bufferName,
			Mark:       *mark,
		},
		`(let ((buffer))
           {{if .BufferName}}
             (setq buffer (get-buffer {{str .BufferName}}))
           {{else}}
             (setq buffer
               (catch 'ecat-return 
                 (dolist (buffer (buffer-list))
                   (when (not (eq 'shell-mode (with-current-buffer buffer major-mode)))
                     (throw 'ecat-return buffer)))))
           {{end}}
           (unless buffer (error "No suitable  buffer found"))
           (with-current-buffer buffer
             (let ((start (point-min)) (end (point-max)))
               {{if .Mark}}
                 (when (region-active-p)
                     (setq start (min (point) (mark))
                           end   (max (point) (mark))))
               {{end}}
               (buffer-substring-no-properties start end))))`); err != nil {
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
