package main

import (
	"flag"
	"fmt"
	"log"
	"net"
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
	tty := false
	flag.BoolVar(&tty, "tty", false, "If true, run ediff in the current terminal.")
	frame := false
	flag.BoolVar(&frame, "frame", false, "If true, run ediff in a new frame.")

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

	usingTTY := false
	if frame {
		err = emacsclient.SendCreateFrame(c)
		if err != nil {
			log.Fatal(err)
		}
	} else if tty && os.Getenv("INSIDE_EMACS") == "" {
		// Refuse to take over the TTY when run from inside emacs.
		err = emacsclient.SendTTY(c)
		if err != nil {
			log.Fatal(err)
		}
		usingTTY = true
	}

	err = emacsclient.SendEvalFromTemplate(
		c, args, `(catch 'ec-return
  (require 'ediff)
  (require 'cl)
  (lexical-let ((resolved-merge nil)
                (keep-merged-buffer nil)
                (saved-window-config (current-window-configuration))
                (saved-ediff-quit-merge-hook ediff-quit-merge-hook))

    (let ((buf (find-buffer-visiting {{str .Merged}})))
      (when (buffer-live-p buf)
        (setq keep-merged-buffer t)
        (kill-buffer buf))
      (when (buffer-live-p buf) (throw 'ec-return "aborted")))

    (let ((base {{str .Base}})
          (local {{str .Local}})
          (remote {{str .Remote}})
          (merged {{str .Merged}})
          (setup-func (list (lambda ()
             (let ((quit-success
                    (lambda ()
                      "Quit ediff, reporting the merge as having succeeded."
                      (interactive)
                      (ediff-barf-if-not-control-buffer)
                      (setq resolved-merge
                            (let ((c 0))
                              (dotimes (n ediff-number-of-differences)
                                (when (and (not (ediff-merge-region-is-non-clash n))
                                           (not (ediff-merge-changed-from-default-p n 'prefers-too)))
                                  (setq c (1+ c))))
                              (zerop c)))
                      (ediff-really-quit nil)))
                   (quit-fail
                     (lambda ()
                       "Quit ediff, reporting the merge as having failed."
                       (interactive)
                       (ediff-barf-if-not-control-buffer)
                       (ediff-really-quit nil)))
                   (quit-signal
                     (lambda ()
                       "Let caller know ediff is done"
                       (when (buffer-live-p ediff-buffer-C)
                         (with-current-buffer ediff-buffer-C
                           (save-buffer)
                           (unless (or keep-merged-buffer (buffer-modified-p))
                             (kill-buffer)))
                       (signal-process {{.Pid}} (if resolved-merge 'SIGUSR1 'SIGUSR2)))))
                   (cleanup
                     (lambda()
                       "Cleanup session."
                       (ediff-janitor nil t)
                       (dolist (buf (list ediff-buffer-A ediff-buffer-B
                                          ediff-ancestor-buffer ediff-control-buffer))
                         (when buf (kill-buffer buf)))
                       (setq-default ediff-quit-merge-hook saved-ediff-quit-merge-hook)
                       (set-window-configuration saved-window-config))))

              (setq-local ediff-autostore-merge t)
              (setq-local ediff-merge-store-file {{str .Merged}})
              (local-set-key [remap ediff-quit] quit-success)
              (local-set-key (kbd "C-c C-c") quit-success)
              (local-set-key (kbd "C-c C-k") quit-fail)
              (add-hook 'ediff-cleanup-hook cleanup -10 'local)
              (remove-hook 'ediff-quit-merge-hook 'ediff-maybe-save-and-delete-merge)
              (add-hook 'ediff-quit-merge-hook quit-signal 100 'local))))))

  (select-frame-set-input-focus (selected-frame))
  (if (or (equal "" base) (not (file-exists-p base)))
      (ediff-merge local remote setup-func merged)
    (ediff-merge-with-ancestor local remote base setup-func merged))
  "success")))`)
	if err != nil {
		log.Fatal(err)
	}
	responses := make(chan emacsclient.Response, 1)
	go emacsclient.Receive(c, responses)
	result, err := emacsclient.ReadToString(responses)
	if err != nil {
		emacsclient.WriteError(err, os.Stderr)
		os.Exit(1)
	}
	if result != "success" {
		if usingTTY {
			resetTTY(c, clientOptions)
		}
		fmt.Fprintf(os.Stderr, "ediff: %s\n", result)
		os.Exit(1)
	}
	sig := <-sigs
	if sig != syscall.SIGUSR1 {
		if usingTTY {
			resetTTY(c, clientOptions)
		}
		fmt.Fprintf(os.Stderr, "ediff: conflicts remain in %s\n", path.Base(args.Merged))
		os.Exit(1)
	}
	if usingTTY {
		resetTTY(c, clientOptions)
	}
	os.Exit(0)
}

// resetTTY attempts to make sure Emacs has stopped using the TTY.
//
// When we close the connection, Emacs clears the TTY it was using.
// However, since this happens on the Emacs process, there's no
// obvious way of making sure Emacs is done - except having Emacs
// answer to another connection since everything happens on the same
// thread.
func resetTTY(currentConn net.Conn, clientOptions *emacsclient.Options) {
	currentConn.Close()

	dummyConn, err := emacsclient.Dial(clientOptions)
	if err != nil {
		log.Fatal(err)
	}
	defer dummyConn.Close()
	err = emacsclient.SendEval(dummyConn, "t")
	responses := make(chan emacsclient.Response, 1)
	go emacsclient.Receive(dummyConn, responses)
	emacsclient.ReadBool(responses)
}
