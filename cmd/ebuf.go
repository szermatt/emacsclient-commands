package main

import (
	"flag"
	"fmt"
	"log"
	"os"

	"github.com/szermatt/emacsclient"
)

func main() {
	type templateArgs struct {
		Mode     string
		NoSelect bool
		Follow   bool
		Reuse    bool
		Name     string
		Fifo     string
	}
	args := &templateArgs{
		Name: "ebuf",
	}

	clientOptions := emacsclient.OptionsFromFlags()
	defineStringFlag(&args.Mode, "m", "mode", "Mode to switch to once file has been read.")
	defineBoolFlag(&args.NoSelect, "s", "noselect", "Don't select the buffer.")
	defineBoolFlag(&args.Follow, "f", "follow", "Keep showing end ouf output.")
	defineBoolFlag(&args.Reuse, "u", "reuse", "Reuse existing buffer, if inactive.")
	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "ebuf displays stdin into an emacs buffer.\n")
		fmt.Fprintf(os.Stderr, "usage: ebuf {args} [buffer-name]\n")
		flag.PrintDefaults()
		fmt.Fprintf(os.Stderr, "  buffer-name\n")
		fmt.Fprintf(os.Stderr, "    \tName of the new buffer (defaults ebuf)\n")
	}
	flag.Parse()

	switch len(flag.Args()) {
	case 0:
	case 1:
		args.Name = flag.Args()[0]
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

	if args.Fifo, err = emacsclient.StdinToFifo(); err != nil {
		log.Fatal(err)
	}

	if err := emacsclient.SendEvalFromTemplate(
		c, args,
		`(let ((buffer ({{if .Reuse}}get-buffer-create{{else}}generate-new-buffer{{end}}
                        {{str .Name}}))
               (pwd default-directory))
           (when-let ((p (get-buffer-process buffer)))
             (when (process-live-p p)
               (error "a process is still active for '%s'" {{str .Name}})))
           (with-current-buffer buffer (delete-region (point-min) (point-max)))
           (setq default-directory pwd)
           (let ((process (start-process {{str .Name}} buffer "cat" {{str .Fifo}})))
             (set-process-filter process
               (lambda (process string)
                 (with-current-buffer (process-buffer process)
                   (unless (process-mark process)
                     (set-marker (process-mark process) (point-min)))
                   (let ((was-at-end (equal (point)
                                            (marker-position (process-mark process)))))
                     (unless (process-get process 'window)
                     {{if .NoSelect}}
                       (display-buffer (current-buffer) t)
                     {{else}}
                       (pop-to-buffer (current-buffer) t)
                     {{end}}
                     (process-put process 'window t))
                     (save-excursion
                       (goto-char (process-mark process))
                       (insert string)
                       (set-marker (process-mark process) (point)))
                       {{if .Follow}}(when was-at-end (goto-char (process-mark process))){{end}}
                       (when (window-live-p (get-buffer-window))
                         (force-window-update (get-buffer-window)))))))
             (set-process-sentinel process
               (lambda (process event)
                 (if (string-equal event "finished\n")
                     (if (process-get process 'window)
                         (with-current-buffer (process-buffer process)
                           (save-excursion
                             {{if .Mode}}({{.Mode}}-mode){{else}}(set-auto-mode t){{end}}))
                       ;; no data, kill the useless buffer
                       (message "%s: no data" process)
                       (kill-buffer (process-buffer process)))
                   ;; not finished, probably an error
                   (message "%s: %s" process event))
                 (delete-file {{str .Fifo}}))))
             (buffer-name buffer))`); err != nil {

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

func defineStringFlag(value *string, short string, long string, description string) {
	flag.StringVar(value, short, *value, "Shorthand for --"+long)
	flag.StringVar(value, long, *value, description)
}

func defineBoolFlag(value *bool, short string, long string, description string) {
	flag.BoolVar(value, short, *value, "Shorthand for --"+long)
	flag.BoolVar(value, long, *value, description)
}
