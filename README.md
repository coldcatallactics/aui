# AUI

A native window onto [Autolith](https://github.com/lambda-symbolics/autolith)'s
world of channels, written in Common Lisp on
[Lightfast](https://github.com/lambda-symbolics/lightfast) and FLTK 1.4. It runs
on Linux and macOS.

AUI is a client of Autolith's world server in the way Autolith's own terminal
is. It loads Autolith's client code into its image, opens channels with
`world-client-open`, follows them as a structured client, and shows the tree
of channels from Autolith's own sidebar model. Nothing is re-derived through a
bridge, so what the terminal can reach, the window can reach.

The look follows Interlisp Paper and the Autolith site: one paper ground,
black ink, Times for what is said, a typewriter face for what the interface
says about it, inversion in place of grey, dither as the only in-between tone,
hairlines, no rounded corners.

![A conversation: the channel list, an opened edit, an answer](assets/pictures/conversation.png)

![A new channel](assets/pictures/welcome.png)

![An approval waiting above the composer](assets/pictures/approval.png)

## Install

AUI is built on the machine it runs on.

1. Install Autolith from a source checkout and run its `script/bootstrap`, so
   that `autolith` is on your path and has run once. AUI builds against that
   checkout and uses its SBCL and its pinned libraries.
2. Install FLTK 1.4 and a C++17 compiler. On macOS: `brew install fltk`. On
   Arch: `pacman -S fltk`.
3. Clone Lightfast beside AUI:

   ```sh
   git clone https://github.com/lambda-symbolics/lightfast.git
   git clone https://github.com/lambda-symbolics/aui.git
   ```

4. Build and install:

   ```sh
   cd aui
   script/install
   ```

`script/install` builds Lightfast's native bridge and the saved image
`build/aui`, links `aui` into `~/.local/bin`, and on macOS also makes
`~/Applications/Autolith.app` for the Dock and Spotlight. After updating AUI,
Lightfast or Autolith, run `aui` again: the launcher rebuilds the image when
it is older than any of them.

`AUI_AUTOLITH_ROOT` names the Autolith checkout when `autolith` is not on the
path, and `AUI_LIGHTFAST_ROOT` names Lightfast when it is not beside AUI.

On a Mac with Homebrew, from nothing, these are the steps the macOS check in
`.github/workflows/check.yml` runs on every push:

```sh
brew install sbcl fltk rust cmake pkg-config openssl coreutils gnu-tar
export PATH="$(brew --prefix coreutils)/libexec/gnubin:$HOME/.local/bin:$PATH"

curl -LO https://beta.quicklisp.org/quicklisp.lisp
sbcl --non-interactive --load quicklisp.lisp --eval '(quicklisp-quickstart:install)'

git clone https://github.com/lambda-symbolics/autolith.git
(cd autolith && ./script/bootstrap)
mkdir -p ~/.local/bin && ln -s "$PWD/autolith/bin/autolith" ~/.local/bin/autolith
autolith auth                 # sign in to a model provider, once

git clone https://github.com/lambda-symbolics/lightfast.git
git clone https://github.com/lambda-symbolics/aui.git
(cd aui && script/install)
```

After that, Autolith is in `~/Applications`, and `aui` is on the path.

## Run

`aui` takes `autolith`'s command line.

```sh
aui                          # the latest channel for this directory, or a new one
aui open ID                  # a specific channel, by identifier or a part of it
aui open                     # no channel yet: pick one from the list
aui --model MODEL --permissions auto
aui -C ~/work/project        # as if started in that directory
aui lg status                # every other sub-command runs as under autolith
```

The first `aui` starts the world server if none runs, exactly as the first
`autolith` does. Options that concern the channel (`--model`, `--provider`,
`--permissions`, `--immutable`, `--image`, `--site-config-root`) are Autolith's
own option objects, read by Autolith's own functions and sent to the server as
a terminal sends them. Starts that have no window (`--detached`, `--pristine`)
and every sub-command other than `open` are handed to the `autolith` program
unchanged: the process becomes `autolith`, so they run in Autolith's own image
with the person's private definitions as Autolith applies them.

Options of AUI's own:

| option | meaning |
|---|---|
| `-C`, `--directory DIR` | work in DIR instead of the current directory |
| `--prompt TEXT` | send TEXT as soon as the channel is attached |
| `--draft TEXT` | put TEXT in the composer |
| `--view NAME` | show `commands`, `settings` or `channels` at start |
| `--demo` | a scripted conversation, no server needed |
| `--shot FILE` | write a picture of the window and exit |
| `--size WIDTHxHEIGHT` | with `--shot`, draw offscreen at exactly this size |

## Using it

The left side lists the world's channels: the inbox, then each workspace with
its channels, children under their parents. A green dot marks an agent at
work, a ring a hosted channel that is idle, a small point one nobody serves.
Counts at the right are unread messages and mentions. Click a channel to
attach to it; right-click for more.

Write in the composer. A draft starting with `/` or `(` is a command or a Lisp
form and runs as it does in the terminal; typing either offers the channel's
commands, and `@` offers the world's participants. The chips under the text
choose the model, the reasoning effort and the permission mode.

What the agent did is shown as one line per tool call, reasoning step or
notice; click a line to open it. Drag across the conversation to select and
copy text.

| key | action |
|---|---|
| Enter | send; while the agent works, steer the running turn |
| Shift+Enter | break the line |
| Tab | take the highlighted completion; while the agent works, queue a follow-up |
| Up, Down | step through completions, or through what you sent |
| Escape | put a list or the drawer away; otherwise stop the agent |
| 1 to 9 | answer a waiting approval with that choice, when the composer is empty |
| Ctrl+K | commands |
| Ctrl+, | settings |
| Ctrl+T | show or hide the channel list |
| Ctrl+O | find a channel |
| Ctrl+N | a new channel in this directory |
| Page Up, Page Down | scroll the conversation |
| Ctrl+Home, Ctrl+End | its beginning; its end, following new output again |

On macOS the Command key takes the place of Ctrl.

## Develop

```sh
script/check                    # tests, source rules and the warning compile
script/check --suite records    # one suite
script/check --fix              # repair what the source rules can repair
script/run --demo               # the scripted conversation, from source
script/run --demo --size 1400x900 --shot /tmp/aui.png
script/exercise                 # click, scroll, select and type against the demonstration
script/exercise-live            # real turns through a world server of its own and a scripted model
script/build                    # the saved image
```

The source rules are [cheque](https://github.com/lambda-symbolics/cheque)'s,
installed on first use into the per-user cache. The checks run on macOS in
GitHub Actions for every push, and keep pictures of the demonstration drawn
there as an artifact.

`script/exercise-live` needs no account. It makes a world under `/tmp`, starts
Autolith's world server for it, registers a model that is a script in the same
process, and does what a person does in the window: send, queue a follow-up,
allow a command, stop a turn, steer one, run a command, reopen the channel
from its history. Then it stops the server and removes the world.

`src/client/` is the only directory that names Autolith's definitions. When
Autolith publishes its client interface, that directory is what changes.
