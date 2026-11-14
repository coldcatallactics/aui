# Repository Guidelines

AUI follows the conventions of Autolith's `AGENTS.md`: one package `#:aui`,
`:use` only `#:cl`, Serapeum's `->` for function types, documentation on every
definition, kebab-case names with entity prefixes and double-hyphen internals,
keyword arguments past three positional parameters, quoted keyword data, typed
conditions instead of error strings, no TODOs or stubs, no em dashes, and tiny
imperative title-only commits without attribution trailers, pushed after each
one. Read that file in the Autolith checkout before changing anything here.
cheque enforces the mechanical part; `script/check` must pass before a commit.

## What AUI is

A client of Autolith's world server, as Autolith's terminal is. It loads the
`autolith` system and uses its client code directly: `world-client-open`,
`channel-subscription`, `world-sidebar`, the conversation log reader, and
Autolith's own command-line option objects. Do not reimplement anything
Autolith's client side already does, and do not edit the Autolith checkout:
ask for what is missing.

## Layout

- `src/model/`: the transcript, the event queue and the Markdown reader. Pure;
  no display, no Autolith.
- `src/client/`: everything that names an Autolith definition. The world, a
  channel session, history reading, the projection of records and live events
  onto the transcript, and the command line.
- `src/ui/`: the toolkit over Lightfast. Theme, painting, the stage with its
  pointer regions, paragraphs and blocks, controls.
- `src/view/`: the window. How entries are typeset, the document, the
  channel list, the composer, completion, overlays, and the workbench that
  holds the window's state.
- `tests/`: FiveAM, same package, no display, server or credentials.
- `script/setup.lisp`: finds the Autolith checkout and its dependency set;
  every script and the source checks load it first.

## Checks

```sh
script/check                    # everything
script/check --suite records    # one suite, no source checks
script/check --fix              # repair layout findings
script/exercise                 # drive the demonstration offscreen; needs a display
script/exercise-live            # drive real turns against a scripted model in a world of its own
script/run --demo --size 1400x900 --shot /tmp/aui.png
```

A change to what the client sends or how live events are shown gets
`script/exercise-live`, which needs no account and touches nobody's world.
A visual change gets a picture: `--shot` with `--size` draws offscreen at that
size without showing a window, so it works under a tiling compositor and on a
build machine. Develop against a pinned Autolith checkout named by
`AUI_AUTOLITH_ROOT` when the installed one is being edited, and against a
world of your own (`XDG_DATA_HOME`, `XDG_STATE_HOME`, `XDG_CONFIG_HOME` pointed
elsewhere) rather than the person's.

## Boundaries

The interface thread alone touches widgets, the transcript and the workbench.
Client threads post immutable `ui-event`s to the queue and wake it. Every
handler that FLTK calls runs under `with-stage-guard`, so an error is reported
in the window instead of unwinding through foreign frames.

Everything visible is drawn by AUI on one stage; FLTK widgets appear only
where a person types. Lightfast stays general purpose: what AUI needs from
FLTK goes into Lightfast as a reusable operation, never as application
behavior.

The look is Interlisp Paper's with the Autolith site's liberties: one paper
ground, ink, the brand green only for what is alive, inversion for selection
and hover, dither for shadows and de-emphasis, hairlines, no rounded corners,
no grey, sentence case.
