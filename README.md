# Introduction

This is a collection of small shell utilities that connect to a local
Emacs server. 

These utilities make it easy for shell commands and scripts to
integrate with Emacs. This is especially useful when running from a
shell from within an Emacs instance.

These tools connect to Emacs through a UNIX socket created by the
[Emacs server]

[Emacs server]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Emacs-Server.html

## Getting Started

Assuming that you have [golang tools](https://golang.org/doc/install) installed, just run:

```shell
make 
sudo make install
```

and you'll find the commands in `/usr/local/bin`. 

To install it in your home directory instead, run:

```shell
DESTDIR=~/ make install
```

## Commands

### e

e evaluates a lisp expression and prints the result.

The main difference between e and [`emacsclient -eval`] is that e cleans
up the result, so you'll get:

```
$ e '(concat "hello " "world")'
hello world
```

and not `"hello world"`. This kind of output is easier to use in
shell scripts.

[`emacsclient -eval`]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Invoking-emacsclient.html

### epipe

`epipe` allows piping short command output to the shell and large
command output into an Emacs buffer.

For example:
```
git log | epipe
```
outputs the first 15 lines in the shell, then redirects the rest to a
new buffer.

You can configure the number of lines to output in the shell, the
buffer name and whether a new buffer is created every time. See the
output of `epipe -help` for details.

`epipe` makes a good default pager, when running in a shell inside
of emacs. Try adding the following to your `.bashrc`:

```shell
if [ -n "$INSIDE_EMACS" ]; then
  export PAGER="epipe --limit 30" 
  export GIT_PAGER="epipe --limit 10 '*git*'"
fi
```

In some cases, Emacs can detect the type of content that is piped and
choose the appropriate major mode. This can be configured on the Emacs
side. See the documentation of `magic-mode-alist` in the section [Auto
Major Mode] of the Emacs manual.

[Auto Major Mode]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Auto-Major-Mode.html

For example, with:

```elisp
(push '("^diff" . diff-mode) magic-mode-alist)
```

Emacs will detect diffs and format them nicely with `diff-mode` when
using `git diff | epipe`


### ebuf

`ebuf` allows piping the output of shell commands to a new Emacs
buffer.

Example:

```shell
grep cat ~/.bash_history | ebuf cats
```

`ebuf` works like `epipe`, with the difference that even short command
output are put into the buffer.

### erun

`erun` tells Emacs to execute a command inside of a buffer. This can
be more convenient than piping and works with interactive commands. 

You can customize the buffer name, whether a buffer is created,
whether the buffer is selected and whether the point stays at the
beginning or follows the end of the file. See the output of `erun
-help` for details.

Example:

```shell
erun grep cat ~/.bash_history
```

### ecat

`ecat` outputs a buffer to stdout. The buffer is identified by its
name. 

This lets you process the content of buffers from inside the shell.

Example:

```shell
ecat cats | wc -l
```

### ereg

`ereg` can both set and read the content of the kill ring.

`ereg` alone outputs the content of the kill ring, while `ereg -i`
puts stdin into the kill ring.

Example:

```
$ echo hello | ereg -i
$ ereg
hello
```

Add the name of a register to read or write to a register instead of
the kill ring.

### ecompile

`ecompile` executes a command in a compilation buffer.

Example:

```shell
ecompile make
```

### emerge

`emerge` helps resolve conflicts using ediff. 

The command starts an ediff merge session and waits for the user to end
the session, usually by pressing q in the buffer "Ediff Control Panel". 

If the session ends with all conflicts resolved, the merge is reported
as successful to the caller. If there are conflicts left, the session
is reported as failed.

Normally, `emerge` reuses Emacs current frame, to change that specify
either `-tty` to run ediff in the current terminal or `-frame` to open
a new frame.

The tool also installs the following command bindings to the Ediff
control panel:

C-c C-k
: ends the ediff session unsuccessfully

C-c C-c
: ends the ediff session successfully

To use it with git, add the following to `~/.gitconfig`:

```
[mergetool "ediff"]
    cmd = /usr/local/bin/emerge -local "$LOCAL" -remote "$REMOTE" -base "$BASE" -merged "$MERGED"
    trustExitCode = true

[merge]
    tool = ediff
```


To use it with Mercurial, add the following to `~/.hgrc`:

```
[ui]
merge = emacs

[merge-tools]
emacs.executable = /usr/local/bin/emerge
emacs.args = -local $local -remote $other -base $base -merged $output
```

## Connection to Emacs

These tools communicate with Emacs through a UNIX socket. The
default matches the default socket name created by `server-start`.

If you use more than one server, you'll need to either pass it to the
command, using the argument `-socket-name` or set the env variable
`EMACS_SOCKET_NAME`.

Connections through TCP are not supported at this time.

---

License GPL2
