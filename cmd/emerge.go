package main

import (
	"flag"
	"fmt"
	"log"
	"os"
	"os/signal"
	"path"
	"path/filepath"
	"syscall"

	"github.com/szermatt/emacsclient"
)

func main() {
	type templateArgs struct {
		Local  string
		Remote string
		Merged string
		Base   string
		Pid    int
	}
	args := &templateArgs{
		Pid: os.Getpid(),
	}

	clientOptions := emacsclient.OptionsFromFlags()
	flag.StringVar(&args.Local, "local", "", "Temporary file containing the contents of the file on the current branch")
	flag.StringVar(&args.Remote, "remote", "", "Temporary file containing the contents of the file to be merged")
	flag.StringVar(&args.Merged, "merged", "", "Name of the file to which the merge tool should write the result of the merge resolution")
	flag.StringVar(&args.Base, "base", "", "Temporary file containing the common base for the merge. Optional.")
	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "emerge merge files using ediff on Emacs.\n")
		fmt.Fprintf(os.Stderr, "usage: ediff {args}\n\n")
		flag.PrintDefaults()
		execPath, err := filepath.Abs(os.Args[0])
		if err != nil {
			execPath = "emerge"
		}
		fmt.Fprintf(os.Stderr, `
This command is meant to be called by git or hg as merge tool.
Examples:

.hgrc:

[ui]
merge = emacs

[merge-tools]
emacs.executable = %s
emacs.args = -local $local -remote $other -base $base -merged $output

.gitconfig:

[mergetool "ediff"]
    cmd = %s -local "$LOCAL" -remote "$REMOTE" -base "$BASE" -merged "$MERGED"
    trustExitCode = true

[merge]
    tool = ediff
`, execPath, execPath)
	}
	flag.Parse()

	if len(args.Local) == 0 || len(args.Remote) == 0 || len(args.Merged) == 0 {
		fmt.Fprintf(os.Stderr, "ERROR: Local, remote and merged files must be defined.\n")
		flag.Usage()
		os.Exit(3)
	}

	c, err := emacsclient.Dial(clientOptions)
	if err != nil {
		log.Fatal(err)
	}
	defer c.Close()

	sigs := make(chan os.Signal, 1)
	signal.Notify(sigs, syscall.SIGUSR1, syscall.SIGUSR2)

	err = emacsclient.SendEvalFromTemplate(
		c, args, `(progn
(require 'ediff)
(require 'cl)
(let ((base {{str .Base}})
      (local {{str .Local}})
      (remote {{str .Remote}})
      (merged {{str .Merged}})
      (setup-func (list (lambda ()
        (lexical-let ((ec-resolved nil))
          (let ((quit-success
                 (lambda ()
                   "Quit ediff, reporting the merge as having succeeded."
                   (interactive)
                   (ediff-barf-if-not-control-buffer)
                   (setq ec-resolved
                         (let ((c 0))
                           (dotimes (n ediff-number-of-differences)
                             (when (and (not (ediff-merge-region-is-non-clash n))
                                        (not (ediff-merge-changed-from-default-p n 'prefers-too)))
                               (setq c (1+ c))))
                           (zerop c)))
                   (call-interactively 'ediff-quit)))
                (quit-fail
                  (lambda ()
                    "Quit ediff, reporting the merge as having failed."
                    (interactive)
                    (call-interactively 'ediff-quit)))
                (quit-signal
                  (lambda ()
                    "Let caller know ediff is done"
                    (signal-process {{.Pid}} (if ec-resolved 'SIGUSR1 'SIGUSR2)))))
          (local-set-key [remap ediff-quit] quit-success)
          (local-set-key (kbd "C-c C-c") quit-success)
          (local-set-key (kbd "C-c C-k") quit-fail)
          (add-hook 'ediff-quit-hook quit-signal 0 'local)))))))

  (if (or (equal "" base) (not (file-exists-p base)))
      (ediff-merge local remote setup-func merged)
    (ediff-merge-with-ancestor local remote base setup-func merged))))`)
	if err != nil {
		log.Fatal(err)
	}

	responses := make(chan emacsclient.Response, 1)
	go emacsclient.Receive(c, responses)
	err = emacsclient.ConsumeAll(responses)
	if err != nil {
		emacsclient.WriteError(err, os.Stderr)
		os.Exit(1)
	}

	sig := <-sigs
	if sig != syscall.SIGUSR1 {
		fmt.Fprintf(os.Stderr, "ediff: conflicts remain in %s\n", path.Base(args.Merged))
		os.Exit(1)
	}
	os.Exit(0)
}
