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

## Autolith client

Use the `autolith` system for world connections, channel subscriptions,
sidebar data, conversation logs and command-line parsing. If a client operation
is missing, ask for it upstream. Do not edit the Autolith checkout.

## Layout

- `src/model/`: transcript data, the event queue, Markdown, Lisp formatting
  and local preferences. Keep this code independent of Autolith and the display.
- `src/client/`: world connections, sessions, history and launch options.
  Convert Autolith records and live events to transcript entries here.
- `src/ui/`: shared Lightfast drawing and input controls.
- `src/view/`: window state, panes, overlays and transcript layout.
- `src/unslop/`: an editor for prose in Lisp checkouts. Keep it independent
  of the Autolith client and views, in both directions. In `proof.lisp`, check
  that an edit changes only comments or string contents.
- `tests/`: FiveAM tests. Run them without a display, server or credentials.
- `script/setup.lisp` and `script/autolith.sh`: find the Autolith installation
  and its dependencies for Lisp and shell entry points.

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

Run `script/exercise-live` after changes to client requests or live events. The
script uses a separate world and needs no account. After toolkit changes, run
both `script/exercise` and `script/exercise-unslop`.

Take a screenshot after visual changes. Use `--shot` with `--size` for a
capture at fixed dimensions without opening a visible window.

To work against a particular Autolith checkout, set `AUI_AUTOLITH_ROOT` to
its path. For manual tests, set `XDG_DATA_HOME`, `XDG_STATE_HOME` and
`XDG_CONFIG_HOME` to separate test directories.

## Threads and Autolith

Access widgets, the transcript and the workbench only on the interface thread.
From a client thread, post an immutable `ui-event` and wake the interface
thread. Wrap FLTK callbacks in `with-stage-guard` so errors appear in the
window instead of unwinding through foreign frames.

Support both release installations and source checkouts. Keep release path
lookup in `script/autolith.sh`, `script/setup.lisp` and
`src/client/installation.lisp`. Use the installed `autolith` launcher for the
world server and terminal commands so they start in the correct runtime.

## Drawing

Draw the window on one stage. Use FLTK widgets for text entry. If you need a
new FLTK operation, add it to Lightfast and keep application behavior in AUI.

Follow the Interlisp Paper style used on the Autolith website:

- one paper ground and ink
- inversion for selection and hover
- dither for shadows and de-emphasis
- hairlines
- no rounded corners and no grey
- sentence case

Use the same accent colors as the terminal:

- green for what is alive or went well
- red for what failed
- amber for tools and warnings
- blue for code among prose, for links and for the current step
- plum for defining words in code

Use the ink tokens in `src/ui/theme.lisp`. These have lighter variants for
inverted backgrounds.

## Words in the window

In tooltips, notices and the README, describe what the user can do or state a
fact. Write “Stop (Esc)” instead of “Esc stops”.

Use lowercase text without a final period for transcript notices, errors above
the composer and the status line. Omit the notice mark: the mark and ink depend
on the tone. Pass plain text to `(setf workbench-notice)`.

Use sentence case for titles, headings, buttons and menus. Keep labels marked
ALL-CAPS in capitals.

## Overlays and layout

Draw menus, pickers, prompts and panels on the stage (`src/view/overlay.lisp`)
with the same faces as the rest of the window. Use the native file chooser.
Dim the underlying overlay if another opens above it.

Add panes in `window--layout`, where all window sizes are handled. In wide
windows the console sits beside the conversation. In narrow windows the title
and activity appear around it. Put the composer, pending input and status line
below the conversation in both layouts. Use a full-width channel list if it
does not fit beside the conversation.

Save window preferences through `preference` in `src/model/preferences.lisp`.
Store Autolith settings in Autolith.
