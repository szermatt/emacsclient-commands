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
	type templateArgs struct {
		Ansi        bool
		NoSelect    bool
		Reuse       bool
		Name        string
		Env         []string
		Command     string
		CommandArgs []string
	}
	noshell := false
	args := &templateArgs{
		Name: "ebuf",
	}

	clientOptions := emacsclient.OptionsFromFlags()
	defineBoolFlag(&args.Ansi, "t", "ansi", "Turn on ansi mode to get a real terminal.")
	defineBoolFlag(&args.Reuse, "u", "reuse", "Reuse existing buffer, if inactive.")
	defineBoolFlag(&args.NoSelect, "s", "noselect", "Don't select buffer after it's been created.")
	defineBoolFlag(&noshell, "S", "noshell", "Don't use a shell to evaluate the command.")
	defineStringFlag(&args.Name, "n", "name", "Buffer name.")
	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "erun runs a command in an Emacs buffer.\n")
		fmt.Fprintf(os.Stderr, "usage: erun {args} command...\n")
		flag.PrintDefaults()
		fmt.Fprintf(os.Stderr, "  command\n")
		fmt.Fprintf(os.Stderr, "    \tShell command and arguments\n")
	}
	flag.Parse()

	if len(flag.Args()) == 0 {
		fmt.Fprintf(os.Stderr, "ERROR: Missing command\n")
		flag.Usage()
		os.Exit(3)
	}

	if noshell {
		args.Command = flag.Args()[0]
		args.CommandArgs = flag.Args()[1:]
	} else {
		args.Command = "bash"
		args.CommandArgs = append([]string{"-i", "-c", buildShellCommand(flag.Args())})
	}

	c, err := emacsclient.Dial(clientOptions)
	if err != nil {
		log.Fatal(err)
	}
	defer c.Close()

	if err := emacsclient.SendEvalFromTemplate(
		c, args,
		`(progn
  (require {{if .Ansi}}'term{{else}}'comint{{end}})
  (require 'cl)
  (lexical-let* ((bufname {{str .Name}})
                 (buffer ({{if .Reuse}}get-buffer-create{{else}}generate-new-buffer{{end}} bufname))
                 (process-environment {{strList .Env}})
                 (erun-func))
    (setq erun-func (lambda ()
      (interactive)
      (when-let ((p (get-buffer-process buffer)))
            (when (process-live-p p)
              (error "a process is still active for '%s'" bufname)))
      (with-current-buffer buffer
        (delete-region (point-min) (point-max))
        {{if .Ansi}}
          (term-mode)
          (term-exec buffer (buffer-name) {{str .Command}} nil {{strList .CommandArgs}})
          (term-char-mode)
        {{else}}
          (comint-mode)
          (comint-exec buffer (buffer-name) {{str .Command}} nil {{strList .CommandArgs}})
        {{end}})
        (set-process-sentinel
          (get-buffer-process buffer)
          (lambda (process event)
              (set-auto-mode 'keep-mode-if-same)
              (local-set-key (kbd "C-c g") erun-func)
              (when (memq 'undefined (mapcar 'cdr (minor-mode-key-binding [remap self-insert-command])))
                (local-set-key (kbd "g") erun-func))))))
    (funcall erun-func)
    {{if .NoSelect}}
      (display-buffer buffer t)
    {{else}}
      (pop-to-buffer buffer t)
    {{end}}
    (buffer-name)))`); err != nil {
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

func buildShellCommand(unquoted []string) string {
	var quoted []string
	for _, e := range unquoted {
		quoted = append(quoted, shellescape.Quote(e))
	}
	return strings.Join(quoted, " ")
}
