# AUI

A native window for [Autolith](https://github.com/lambda-symbolics/autolith),
on Linux, macOS and NetBSD. You see the same channels as in Autolith's
terminal and can do the same things with them.

![A conversation in AUI](assets/pictures/conversation.png)

## Install

You need Autolith, FLTK 1.4 and a C++17 compiler.

| system | FLTK |
|---|---|
| macOS | `brew install fltk` |
| Arch | `pacman -S fltk` |
| NetBSD | from pkgsrc: `x11/fltk`, `devel/gmake`, `shells/bash`, `devel/pkgconf` |

```sh
curl -fsSL https://sh.lambda-symbolics.com/autolith | sh   # Autolith, if you do not have it
autolith auth                                              # sign in to a model provider, once

git clone https://github.com/lambda-symbolics/lightfast.git
git clone https://github.com/lambda-symbolics/aui.git
cd aui && script/install
```

`aui` is then in `~/.local/bin`, and on macOS Autolith is in `~/Applications`.
After you update AUI or Autolith, run `aui` as before. That start takes a
minute, because the program is built again.

## Run

```sh
aui                     # the latest channel for this directory, or a new one
aui open                # pick a channel
aui open ID             # a specific channel
aui -C ~/work/project   # as if started in that directory
```

`aui` takes the options and sub-commands of `autolith`.

## Unslop

`unslop DIRECTORY` shows the comments, strings and docstrings of a Common Lisp
checkout, file by file, for rewriting. What you type is saved when you stop
typing. Nothing is committed.

## Keys

| key | action |
|---|---|
| Enter | send; while the agent works, steer it |
| Tab | while the agent works, queue a follow-up |
| Escape | stop the agent |
| `/` or `(` | a command or a Lisp form, as in the terminal |
| `@` | mention someone |
| 1 to 9 | answer an approval |
| Ctrl+K | commands |
| Ctrl+, | settings |
| Ctrl+S | show or hide the channel list |
| Ctrl+O | find a channel |
| Ctrl+N | a new channel |

On macOS use Command instead of Ctrl.

## Develop

```sh
script/check            # tests and source rules
script/exercise         # the demonstration, driven offscreen
script/exercise-live    # real turns against a scripted model; needs no account
script/run --demo --size 1400x900 --shot /tmp/aui.png
```

The rules for changes are in [AGENTS.md](AGENTS.md).
