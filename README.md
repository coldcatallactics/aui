# AUI

Use AUI to open your [Autolith](https://github.com/lambda-symbolics/autolith)
channels in a native window on Linux, macOS or NetBSD.

![A conversation in AUI](assets/pictures/conversation.png)

## Install

You need Autolith, FLTK 1.4 and a C++17 compiler.

| System | Packages |
|---|---|
| macOS | `brew install fltk` |
| Arch | `pacman -S fltk` |
| NetBSD | from pkgsrc: `x11/fltk`, `devel/gmake`, `shells/bash`, `devel/pkgconf` |

```sh
curl -fsSL https://sh.lambda-symbolics.com/autolith | sh   # install Autolith if you do not have it
autolith auth                                              # sign in to a model provider (once)

git clone https://github.com/lambda-symbolics/lightfast.git
git clone https://github.com/lambda-symbolics/aui.git
cd aui && script/install
```

Run `aui` or `unslop` from `~/.local/bin`. On macOS you can also open Autolith
from `~/Applications`. Allow time for a rebuild the first time you run `aui`
after an update.

## Run

```sh
aui                     # open the latest channel for this directory, or a new one
aui open                # pick a channel
aui open ID             # open the channel with this ID
aui -C ~/work/project   # start as if in that directory
```

Use the usual Autolith options. Commands such as `aui auth` run in the terminal.

## Keys

| Key | Action |
|---|---|
| Enter | send, or steer the agent while it works |
| Tab | queue a follow-up while the agent works |
| Escape | stop the agent |
| `(` | type a command or a Lisp form, as in the terminal |
| `@` | mention someone |
| 1 to 9 | answer an approval |
| Ctrl+K | open the commands |
| Ctrl+, | open the settings |
| Ctrl+S | show or hide the channel list |
| Ctrl+O | find a channel |
| Ctrl+N | start a new channel |

On macOS, use Command instead of Ctrl.

## Unslop

Run `unslop DIRECTORY` to edit comments, strings and docstrings in Common Lisp
source files. Changes are saved in place when you pause typing. Review the diff
before you commit.

## Develop

The three interface exercises need a display, even when the window is hidden.
```sh
script/check            # run the tests and the source checks
script/exercise         # drive the demonstration offscreen
script/exercise-live    # drive real turns against a scripted model (no account needed)
script/exercise-unslop  # edit files in a temporary checkout
script/run --demo --size 1400x900 --shot /tmp/aui.png
```

Read [AGENTS.md](AGENTS.md) before you change AUI.
