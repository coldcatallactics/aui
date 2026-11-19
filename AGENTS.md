# Repository guidelines

AUI follows the conventions in the `AGENTS.md` file of Autolith. Read that file
in the Autolith checkout before you change anything here. In short:

- Use one package, `#:aui`, which only uses `#:cl`.
- Declare function types with the `->` of Serapeum.
- Document every definition.
- Use kebab-case names with entity prefixes. Internal names contain a double
  hyphen.
- Use keyword arguments after three positional parameters.
- Quote keyword data.
- Signal typed conditions instead of error strings.
- Do not leave TODOs or stubs.
- Do not use em dashes.
- Make tiny commits with an imperative title, no body and no attribution
  trailers. Push after each commit.

The mechanical part of these rules is checked by cheque. `script/check` must
pass before a commit.

## What AUI is

AUI is a client of the Autolith world server, like the Autolith terminal. It
loads the `autolith` system and uses the client code of Autolith directly:
`world-client-open`, `channel-subscription`, `world-sidebar`, the conversation
log reader and the command-line option objects of Autolith. Do not reimplement
anything that the client side of Autolith already does. Do not edit the
Autolith checkout. Ask for what is missing instead.

## Layout

- `src/model/`: the transcript, the event queue and the Markdown reader. This
  code is pure. It does not use a display or Autolith.
- `src/client/`: all code that refers to an Autolith definition. This is the
  world, a channel session, history reading, the projection of records and
  live events onto the transcript, and the command line.
- `src/ui/`: the toolkit over Lightfast. This is the theme, painting, the stage
  with pointer regions, paragraphs and blocks, and controls.
- `src/view/`: the window. This is the typesetting of entries, the document,
  the channel list, the composer, completion, the activity list, the parts of
  the console, the overlays with their catalogs, sheets and panels, and the
  workbench, which holds the state of the window.
- `src/unslop/`: a second program on the toolkit, the `unslop` command. It is
  used to rewrite the comments, strings and docstrings of a Common Lisp
  checkout in place. The code in `src/unslop/` does not refer to Autolith,
  `src/client/` or `src/view/`. The code in those places does not refer to
  unslop. The rule of unslop is in `proof.lisp`: edits can change words, and
  they cannot change code.
- `tests/`: FiveAM tests in the same package. They do not need a display, a
  server or credentials.
- `script/setup.lisp`: code used to find the Autolith source and the
  dependencies of Autolith. It is loaded first by every script and by the
  source checks. `script/autolith.sh` is used by the shell scripts to find the
  same Autolith and the SBCL that Autolith runs on.

## Checks

```sh
script/check                    # run all checks
script/check --suite records    # run one suite without the source checks
script/check --fix              # repair layout problems
script/exercise                 # drive the demonstration offscreen (needs a display)
script/exercise-live            # drive real turns against a scripted model in a separate world
script/exercise-unslop          # drive unslop offscreen against a throwaway checkout
script/run --demo --size 1400x900 --shot /tmp/aui.png
```

If you change what the client sends or how live events are shown, run
`script/exercise-live`. It runs in a separate world and needs no account.

If you make a visual change, take a picture. With `--shot` and `--size`, the
window is drawn offscreen at that size and is not shown. This works under a
tiling compositor and on a build machine.

If the installed Autolith is being edited, develop against a pinned Autolith
checkout. Set `AUI_AUTOLITH_ROOT` to that checkout. Develop in a world of your
own instead of the world of the person: point `XDG_DATA_HOME`,
`XDG_STATE_HOME` and `XDG_CONFIG_HOME` elsewhere.

The file `aui.asd` defines three systems:

- `aui/toolkit` is `src/ui/` with the preferences. It can be loaded without
  Autolith.
- `aui` is the client, built on the toolkit.
- `aui/unslop` is unslop, built on the toolkit.

A change to the toolkit changes both programs. After such a change, run
`script/exercise` and `script/exercise-unslop`.

## Threads and Autolith

Only use widgets, the transcript and the workbench on the interface thread.
Client threads post immutable `ui-event`s to the queue and then wake the queue.
Run every handler called by FLTK under `with-stage-guard`. An error in a
handler is then reported in the window. It does not unwind through foreign
frames.

Autolith can be installed from a release or from source. AUI can be built
against both. Facts about the layout of a release are only in
`script/autolith.sh`, `script/setup.lisp` and `src/client/installation.lisp`.
Start the world server and all commands without a window through the installed
`autolith` command, because that command prepares what Autolith needs to run.
Do not start any part of Autolith by a path of your own.

## Drawing

Everything visible is drawn by AUI on one stage. FLTK widgets are only used
where a person types. Lightfast is a general-purpose library. If AUI needs
something from FLTK, add it to Lightfast as a reusable operation. Do not add
application behavior to Lightfast.

The look is the look of Interlisp Paper, with the liberties taken by the
Autolith website:

- one paper ground and ink
- inversion for selection and hover
- dither for shadows and de-emphasis
- hairlines
- no rounded corners and no grey
- sentence case

A few accent inks are used to show what a thing is, as in the Autolith
terminal:

- green for what is alive or went well
- red for what failed
- amber for tools and warnings
- blue for code among prose, for links and for the current step
- plum for defining words in code

The inks are tokens in `src/ui/theme.lisp`. On an inverted ground, they are
made lighter automatically. Do not invent other inks.

## Words in the window

In labels, tooltips, notices and the README, write what a person does, or write
a plain fact. Do not make a key, a window or a list the subject of a verb.
Write "Stop (Esc)", not "Esc stops". Use short sentences and plain words.

Some text is in the voice of the window itself. These are the notices among the
entries, the line above the composer where errors are shown, and the status
line. Write this text as the Autolith terminal writes it:

- Start with a lowercase letter.
- Do not end with a period.
- Do not write the mark in front of a notice. The mark and the ink of the
  notice are added by the code that draws it. A notice entry gets the mark from
  the tone of the notice. Any report is put into this form by
  `(setf workbench-notice)`. Give both of them plain lowercase words.

Keep sentence case in titles, headings, buttons and menus. Keep ALL-CAPS
labels in capitals.

## Overlays and layout

Menus, pickers, prompts and panels are overlays drawn on the stage
(`src/view/overlay.lisp`). Do not use the popups or dialogs of FLTK, because
they do not use the same faces. The file chooser is the one exception.

In a wide window, the console is beside the conversation. The console contains
the title, the activity, the goal of the channel and the channels beneath it,
and the plan, files and messages read from the conversation by
`src/view/digest.lisp`. In a narrow window, the title and the activity are
placed around the conversation. In both layouts, the composer, what waits above
the composer and the status line are under the conversation. If the window is
too narrow for the channel list beside the conversation, the channel list fills
the whole window. All layouts come from `window--layout`. Add a new pane there
once, and it appears in every layout.

An overlay opened from another overlay is drawn over it. The overlay beneath
it is dimmed.

Save the state of the window between runs, such as the width of the console,
through `preference` in `src/model/preferences.lisp`. Keep the settings of
Autolith in Autolith.
