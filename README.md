# AUI

A native graphical workbench for [Autolith](https://github.com/lambda-symbolics/autolith),
written in Common Lisp on [Lightfast](https://github.com/lambda-symbolics/lightfast)
and FLTK 1.4. One substrate, black ink, Times New Roman for prose, CMU
Typewriter Text for code, hairline boxes, and accent colors derived in OKLCH
from the Lambda Symbolics green.

AUI starts and owns a pristine `autolith acp` process for a chosen folder,
shows the conversation as a legible document with collapsible reasoning and
tool calls, answers permission requests, and exposes Autolith's settings,
models, operations and jobs through a property sheet, a command palette and
an inspector.

## Requirements

SBCL, FLTK 1.4 with `fltk-config`, a C++17 compiler, and the Lightfast,
agentcomms, argo and serapeum systems findable by ASDF (a checkout under
`~/common-lisp` does). Lightfast must include the theme and styled text
operations added for AUI, and agentcomms must include the prompt `:on-sent`
callback (commit 7b55dd1 or later). Running live needs an `autolith` on the
path with a credential for the chosen model; `gpt-5.6-luna` is the default.
Screenshots use `grim`, `jq` and the niri compositor.

## Run

```sh
script/run                      # welcome window: choose a folder or a conversation
script/run /path/to/project     # open a workspace there at once
script/run DIR --conversation ID
script/run DIR --demo           # the scripted backend, no Autolith needed
script/check                    # the test suite, headless
```

`--open settings`, `--open palette` and `--open inspector` open that view at
start; `--size WIDTHxHEIGHT` pins the window size so other layouts can be
inspected under a tiling compositor; `--shot PATH` captures the window and
exits after `--shot-after` seconds. Set `AUI_AUTOLITH=/path/to/bin/autolith` to run a development build
of Autolith instead of the one on the path. The design is in
`docs/superpowers/specs/2026-10-09-aui-design.md`.

## Keys

| key | action |
|---|---|
| Ctrl+Enter | send the composer |
| Escape | stop the running turn |
| Ctrl+1 to Ctrl+9 | answer the pending approval with that option |
| Ctrl+K | command palette |
| Ctrl+, | settings sheet |
| Ctrl+I | inspector pane |
| End | follow the end of the transcript again |

Clicking a block's header line expands or collapses it; a right click offers
expand, inspect and copy.
