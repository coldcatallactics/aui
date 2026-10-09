# Repository Guidelines

AUI follows the conventions of Autolith's `AGENTS.md`
(`/root/common-lisp/frob/AGENTS.md`): one package `#:aui`, `:use` only `#:cl`,
Serapeum's `->` for function types, documentation on every definition,
kebab-case names with entity prefixes and double-hyphen internals, keyword
arguments at four or more parameters, quoted keyword data, no TODOs or
stubs, no em dashes, and tiny imperative title-only commits pushed after each
one. Read that file before changing anything here.

## Layout

- `src/theme.lisp`, `src/transcript.lisp`, `src/events.lisp`,
  `src/catalog.lisp`, `src/backend-protocol.lisp`: shared contracts between
  the backend and the views. Change them only after agreement in the spec.
- `src/backend/`: the Autolith process owner, ACP client, extension calls and
  recent-conversation discovery.
- `src/view/`: the windows. `fixtures.lisp` holds the scripted demo backend,
  which is the executable statement of the event contract.
- `tests/`: FiveAM, same package, no display or credentials.

## Checks

```sh
script/check              # all suites
script/check --suite transcript
script/run DIR --demo --shot /tmp/aui.png   # a visual check through niri and grim
```

Run `script/check` before every commit. Visual changes also get a screenshot
at the tile size niri gives the window.

## Boundaries

The FLTK thread alone touches widgets and the transcript model. Backend
threads post immutable `ui-event`s to the queue and call `lightfast:awake`.
Lightfast stays general purpose: anything AUI needs from FLTK goes into
Lightfast as a reusable operation, never as application behavior.
