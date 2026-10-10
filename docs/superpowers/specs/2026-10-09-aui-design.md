# AUI design: an Autolith workbench on FLTK

Date: 2026-10-09. Authors: Claude Fable 5.1 and Autolith session 9 (GPT-6
Astra), as equal partners, from instructions by Lukas Hozda. This document is
the agreed design; it changes by commit, not by conversation.

## 1. Purpose

AUI is a native graphical workbench for Autolith. It starts and owns a
pristine Autolith process for a chosen folder, shows the conversation as a
legible document, exposes every Autolith knob through typed interfaces, and
stays fast and well behaved in a tiling window manager. It is written in
Common Lisp on Lightfast (FLTK 1.4). Nothing in it is web based.

The visual language follows Interlisp Paper (`/root/common-lisp/interlisp-paper`)
in its typography and discipline: one substrate, black ink, Times New Roman
for prose and labels, CMU Typewriter Text for code, hairline boxes, no grays,
no gradients, no bevels, sentence case, keyboard access throughout. Unlike
Interlisp Paper, colored accents are permitted where they increase
readability. The look is modern and flat, not retro: no black title bars,
no dithered shadows inside the application.

## 2. Architecture

```
+-------------------------------------------------------------------+
| AUI process (SBCL)                                                 |
|                                                                   |
|  view/*  (FLTK thread)  <-- ui-queue <--  backend/* (own threads) |
|     |  apply-ui-event                        |  acp-client         |
|     v                                        |  extensions         |
|  transcript model, catalogs                  |  process owner      |
+----------------------------------------------|--------------------+
                                               | stdio JSON-RPC (ACP v1)
                                   +-----------v------------------+
                                   | autolith --pristine acp      |
                                   | one per workspace window     |
                                   | one open conversation        |
                                   +------------------------------+
```

- **Transport.** ACP over stdio (library `agentcomms`) carries prompts,
  streamed assistant text, visible reasoning, tool calls, permission
  requests, cancellation, permission modes, plans and `session/load`. Typed
  `_autolith/*` extension methods and notifications on the same connection
  carry everything ACP does not model: state, catalogs, jobs, settings,
  resources, operation invocation and runtime events. The management REPL
  is an optional developer facility, not a dependency; it does not start in
  ACP mode today.
- **Process ownership.** One `autolith --pristine acp` process per workspace
  window, launched with the chosen folder as its working directory and
  `AUTOLITH_MODEL`/`AUTOLITH_REASONING_EFFORT` overrides. The verified cheap
  route is `gpt-5.6-luna` at low effort. Switching to another folder respawns;
  switching conversation inside a folder closes the current ACP session (after
  the turn ends or is cancelled), resets the view incarnation, then loads or
  creates the next session.
- **Threads.** FLTK runs on the main thread, which is the only thread that
  touches widgets or the transcript model. `agentcomms` runs a reader thread
  for notifications and one handler thread per incoming request. Backend work
  is split so that stop, permission replies and disconnect cleanup never
  queue behind a waiting prompt: a serialized normal-operation queue for
  session changes and mutations, the active prompt wait on its own thread,
  and an independent control path for cancel and shutdown. Notification
  callbacks validate, copy, enqueue and return.
- **Queue.** `*ui-queue*` is a FIFO with O(1) enqueue and a short lock hold.
  The main loop swaps out a bounded batch under the lock, releases it, and
  applies the batch through `apply-ui-event`. Adjacent compatible text
  fragments and superseded state snapshots coalesce; permission and terminal
  events are never dropped. The wakeup function is injectable: production
  uses `lightfast:awake`, headless tests use a no-op.
- **Main loop.** `lightfast:enable-thread-wakeups` once on the main thread,
  then `lightfast:run-with-idle`, which drains the queue before each blocking
  `Fl::wait()` and after each wakeup; the loop ends when the last window
  closes or `quit` is requested.

## 3. Shared contracts

### 3.1 Transcript model (`src/transcript.lisp`, owned by Fable)

A `transcript` holds an ordered vector of `transcript-block`s, an index by
block id, a `turn` counter, and an `incarnation` that increments on every
reset (conversation load or switch). Blocks:

| class | slots beyond the base | notes |
|---|---|---|
| `transcript-block` | `id`, `sequence`, `turn-id`, `received-at`, `backend-time`, `collapsed-p` | `sequence` orders; it is not a durable record number |
| `user-message` | `text`, `attachments` | |
| `assistant-text` | `text` | one block per contiguous text run; a turn may hold several |
| `reasoning` | `text` | collapsed by default |
| `tool-call` | `call-id`, `name`, `kind`, `title`, `status`, `raw-status`, `input`, `output`, `locations`, `artifact-references` | keyed by the session-qualified ACP call id; `status` in `:pending :running :completed :failed :denied :cancelled :interrupted` |
| `plan-block` | `entries` | replaces the previous plan block |
| `approval` | `request-id`, `call-id`, `title`, `arguments`, `directory`, `options`, `decision`, `received-at`, `deadline` | `options` is the list ACP offered, rendered as offered |
| `notice` | `severity`, `text`, `key` | connection, compaction, usage, errors; `key` deduplicates |

Mutations: `transcript-append`, `transcript-append-text`,
`transcript-update-tool-call`, `transcript-replace-plan`,
`transcript-decide-approval`, `transcript-notice`, `transcript-reset`,
`transcript-begin-replay`, `transcript-end-replay`. Each notifies listeners
through `(transcript-listener-changed listener transcript block change)` with
`change` in `:appended :text-appended :updated :replaced :reset :replay-begin
:replay-end`. Rendered byte ranges and style positions belong to the view,
not the model. A block id is stable within one incarnation only.

### 3.2 UI events (`src/events.lisp`, drafted by Fable, owned by AL)

```
ui-event            backend-id, generation, session-id (may be nil), ingress-sequence, received-at
connection-event    state in :starting :ready :lost :closing :exited; detail
session-event       state in :loading :ready :closed; conversation-id, title, directory
turn-event          state in :idle :running :cancelling :interrupted; stop-reason
transcript-event    operation (immutable data describing one transcript mutation)
approval-event      request-id, call-id, title, arguments, directory, options, deadline
state-event         snapshot (from _autolith/state)
jobs-event          kind in :snapshot :delta; jobs; removed-ids
catalog-event       kind in :models :settings :operations :conversations; items
result-event        request-id, outcome in :ok :condition :busy :unsupported :cancelled; value; condition-type, report, data (sanitized strings and data only)
```

Events are immutable; mutable JSON collections are copied before publishing.
The view discards events whose `backend-id`/`generation` is not current.
Replay is bracketed by `session-event :loading` and `:ready`, during which
history mutations are applied without notifications or approval prompts.

### 3.3 Permissions

`client-request-permission` runs on an agentcomms handler thread. It posts an
`approval-event` and waits on a per-request condition variable. The UI
resolves it exactly once with `(backend-decide-approval backend request-id
option-id)`. Turn cancellation, window close or transport loss resolves every
affected waiter as ACP `:cancelled`; a late decision receives a stale result
and changes nothing. Rejection selects an offered reject option. Autolith's
timeout is 120 s; the UI shows an approximate countdown from receipt time
using monotonic time.

### 3.4 `_autolith/*` extensions (designed and implemented by AL in frob)

Method groups and the shapes the views require. Exact names and schemas are
AL's; these are the requirements.

- `capabilities`: implemented method names with schema versions; feature
  flags per method, enabled only when implemented and tested.
- `state` (session): workspace, title, model, effort, permission mode, turn
  state, usage (per request and cumulative, context occupancy separate and
  marked measured or estimated, null when unknown), live job count, plan
  present-p, health.
- `conversations` (cwd optional, cursor, limit): id, display id, title,
  directory, model, created, last activity, user turn count, preview.
- `jobs`, `job`, `job-transcript` (offset/limit in characters), `job-send`,
  `job-cancel`: lineage, role, state, timing, usage, artifacts, last activity.
  Every call carries the owning session; a job id alone is not authority.
- `settings`, `set-setting`: grouped; name, label, documentation, type,
  options, value, source, scope, session-vs-durable, adjustable-p with the
  reason when not (pristine restrictions represented honestly).
- `models`: id, provider, credential stored-p, recently verified-p, context
  window.
- `operations`, `invoke`: commands and tools with description, argument
  schema, admission now, terminal-owning-p (unsupported until a headless
  presenter exists); typed outcomes ok/condition/busy/unsupported/cancelled.
  A queued state means an owned queue entry that can be cancelled.
- `resource` (reads; lines for text, bytes for raw logs; revision, encoding,
  MIME): bounded windows for the inspector. Edits go through a separate
  revision-gated operation.
- Notification `event` with a remote sequence: job lifecycle, usage after each
  request, compaction, pending decisions, and `overflow` meaning "take a fresh
  snapshot". Nothing in-band is durable; side-effecting invocations are
  reconciled after disconnect, never auto-retried.

The plain ACP path works when no extension is available; the UI then reports
those features as unavailable.

Wire shapes agreed on 2026-10-10: every response is an object with `outcome`
(`ok`, `condition`, `busy`, `unsupported`, `cancelled`), `value` on success,
`report` and optional `conditionType` on failure. `_autolith/capabilities`
returns `schemaVersion`, `methods` (exact implemented names) and `features`;
a method-not-found error means plain ACP. Every other call carries
`sessionId`; catalogs return `items` plus optional `nextCursor`. State
carries `workspace`, `title`, `model`, `effort`, `permissionMode`,
`turnState`, `usage` (`input`, `output`, `cached`, `contextUsed`,
`contextLimit`, `contextMeasured`), `liveJobCount`, `planPresent`, `health`.
`set-setting` returns the updated settings item; `invoke` returns `text` and
optional `data`; `job-transcript` returns `content`, `offset`, `nextOffset`,
`total`, `complete`. `_autolith/event` notifications carry `sessionId`,
`sequence`, `kind` (`jobs`, `state`, `compaction`, `decision`, `overflow`,
`turn`) and `data`. A `turn` event carries `state` (`running` or `idle`),
`epoch` (the server's own operation identity), `source` (`prompt`,
`completion` or `invoke`) and `outcome` (null, `end-turn`, `cancelled` or
`condition`); the server sends `running` before any observer or tool callback
and `idle` after the final flush. The client admits a turn the server owns
(automatic continuation or invocation) on `running` when it is idle, so
approvals and Stop work for it, finishes it on the matching `idle`, sends
`session/cancel` alone when stopping it, treats prompt-sourced events as text
boundaries only, and drops events from another session or generation.

Client side (`src/backend/extensions.lisp`, owned by Fable): the backend
probes capabilities right after `initialize`, fetches every provided catalog
once a session is ready, runs reads on their own control threads so they
never wait behind a prompt, sends mutations through the serialized operation
queue so they respect idle admission, and turns `jobs` and `state` events
into refreshes and `compaction` into a notice. Server side (frob, owned by
AL): the `_autolith/*` methods themselves, advertised only once implemented
and tested.

### 3.5 Welcome discovery, two phases

Before any Autolith process exists, `backend/recent.lisp` combines AUI's own
recent cache (paths, conversation ids, drafts; stored under the XDG state
directory) with a bounded read-only reader over Autolith's conversation
header and picker metadata (`*read-eval*` nil, validated shape, tolerant of
truncated records, never parsing message bodies). After a folder is chosen and
the backend is connected, the list is refreshed from `_autolith/conversations`.
AUI never keeps a competing conversation database.

## 4. Lightfast extensions (owned by Fable, general purpose)

Done: `enable-thread-wakeups`/`awake`; `load-font`, `system-font-names`,
`font-name`, `measure-text`; text font and size on text displays, help views,
menus and choices; a `Theme` struct behind every constructor with
`apply-flat-theme` beside `apply-classic-theme` (hairline boxes replace all
bevels globally, one substrate, inverted selection, visible focus).

To do: `run-with-idle` and a blocking `wait`; style tables and styled
appends/replacements on text displays (UTF-8 byte offsets at the boundary);
click position reporting, keyboard copy and scroll control on text displays;
a window screenshot to a file for review by both authors.

## 5. Theme (`src/theme.lisp`)

- Substrate `#FFFEFA`; ink `#000000`. One substrate everywhere.
- Prose and labels: Times New Roman MT Std (regular, bold, italic, bold
  italic) at 16 px. Code: CMU Typewriter Text (four faces) at 15 px. Face and
  size are user preferences of AUI, separate from Autolith settings.
- Spacing scale 4, 8, 12, 16, 24, 32 px. Hairline 1 px. Focus ring 2 px ink.
- Hierarchy through face and spacing, never through gray or size inflation;
  Markdown headings in messages render as bold run-in heads.
- Accents, OKLCH derived from the logo green `#42B28A` (L 0.689, C 0.118,
  h 165.9). Ink tier at L 0.50 for strokes and marks that must read against
  paper; mark tier at L 0.69 for small fills with black labels. Prose is
  never colored.

| role | ink tier | mark tier | meaning |
|---|---|---|---|
| green | `#037555` | `#42B28A` | Autolith, success |
| blue | `#2366A3` | `#5FA0E1` | user, links, information |
| red | `#9B4440` | `#DB7C75` | failure, denied |
| orange | `#935000` | `#D08849` | attention, pending approval |
| yellow | `#7A6000` | `#B79738` | warnings (only when needed) |
| violet | `#784E95` | `#B186D2` | reasoning, sub-agents (only when needed) |

Every colored state also carries a word or a shape. Disabled is a dashed
hairline, not gray. Selected, hovered and pressed states invert ink and paper
atomically.

## 6. Views (owned by Fable)

1. **Welcome.** Wordmark line; "Open a folder" (native chooser) and a path
   field; recent folders; recent conversations (title, folder, model, last
   activity) sorted by activity. Enter resumes, N starts new.
2. **Workbench.** Header strip: folder, title, model, permission mode, turn
   state word. Left rail: this folder's conversations; running jobs with
   state and elapsed time. Transcript. Composer: multiline, draft retained
   per conversation, Ctrl+Enter sends, Escape stops, queued input shown.
   Status line: connection, usage, context occupancy, pending approvals.
   Rail and inspector collapse below 1100 px; everything reflows to any size;
   no restrictive minimum.
3. **Transcript renderer.** One `Fl_Text_Display` with a style table and word
   wrap, fed from the block model. Each block starts with a header line
   (speaker or tool title; time in a gutter or overlay rather than padding).
   Reasoning and tool blocks start collapsed to header plus one summary line.
   A click on the explicit disclosure area toggles; a keyboard action does the
   same for the block at the cursor. FLTK uses one line height per display,
   so all styles share 16 px (serif) and 15 px (mono). Links are underlined
   italic. Images, diffs and long logs open in the inspector. Measure a long
   transcript; fall back to a windowed buffer if needed.
4. **Approvals.** Inline block plus the composer area switching to the
   offered options and countdown. Closing the window or stopping the turn
   resolves as cancelled.
5. **Inspector.** Right pane for the selected tool call, job (transcript,
   send, cancel), artifact or resource window.
6. **Settings sheet, model picker, permission picker.** Property sheets from
   catalogs: value, source, scope, adjustable-p and reason.
7. **Command palette** (Ctrl+K): operations, conversations, settings;
   executes only what is admitted now.

## 7. Repository

```
aui.asd                 systems aui, aui/tests
script/run [DIR] [--conversation ID] [--demo] [--open settings|palette]
                        [--demo-prompt TEXT] [--shot PATH]
                        launch; builds Lightfast's bridge if stale; --demo uses
                        the scripted backend; --shot captures the window and exits
script/shot PATH        capture the newest visible AUI window through niri and grim
script/visual-transcript [--shot]  the sample transcript alone
script/check [--suite S] FiveAM in one SBCL process, no display or credentials
src/package.lisp        one package #:aui, :use #:cl only
src/theme.lisp  src/transcript.lisp  src/events.lisp  src/catalog.lisp
src/backend-protocol.lisp  the generics the views call; *backend-factory* and
                        *recent-conversations-function* are installed by the
                        backend module
src/backend/{process,client,extensions,session,recent}.lisp      (AL)
src/view/{markdown,document,transcript-view,workbench,settings-sheet,
          palette,welcome,fixtures}.lisp                          (Fable)
src/main.lisp
tests/*.lisp            same package
docs/superpowers/specs/ this document
```

The scripted backend in `src/view/fixtures.lisp` is the executable statement of
the event contract: it plays connection, session, replay, turn, approval and
catalog events in the order the views expect, and it drives the demo.

Conventions are frob's `AGENTS.md`: one package, `->` type declarations,
documented definitions, keyword arguments at four or more parameters,
quoted keyword data, no TODOs or stubs, no em dashes, title-only imperative
commits under 72 characters, tiny commits, push after every commit. Both
authors commit to `master` of one checkout: stage exact owned paths, and
serialize commit, pull with rebase and push. Shared contract files and the
spec change only after agreement in the mailbox.

## 8. Acceptance gates

1. Start pristine, initialize ACP, negotiate extensions, target the session.
2. Prompt, inspect a tool call, approve and deny, cancel.
3. Run a child job; verify completion delivery and continuation headlessly.
4. Change model and settings at an admitted boundary.
5. Restart the owned process and load durable history without duplicates,
   showing interrupted state and requiring explicit retry.

Then: screenshots at narrow, ordinary and wide widths with prose, code, a
long path, running and failed tools, an approval, a child job, selected text,
and focused and disabled controls; test selection and copy, keyboard
traversal, scrolling during streaming, and resizing while streaming.

Status on 2026-10-10 01:40: gates 1, 2 and cancellation pass through the
real GUI on `gpt-5.6-luna`; gate 5 replays but two replay defects are open
on the Autolith side (replayed tool statuses, internal messages replayed as
prompts). Screenshots were taken at the 1131 by 2114 tile niri gives the
window; niri's by-id resize actions did not move this window, so other
widths are still to be inspected by hand.

## 9. Slices

1. Lightfast extensions; AUI skeleton, theme, transcript and event models
   with tests; welcome and workbench shells with a fake backend (Fable).
   `events.lisp` ownership transfer, `backend/*` against stock ACP, recent
   discovery, client projection tests (AL). Gates 1 and 2.
2. `_autolith/*` in frob with tests (AL, after 2026-10-10 01:10 CEST);
   status, rail, jobs, settings, models wired to catalogs (Fable). Gates 3, 4.
3. Resume via `session/load`, disconnect handling, inspector, palette.
   Gate 5. Then the screenshot pass and polish together.

Later: `ui.present` and `ui.notify` tools, images in prompts, dedicated
editors for Lisp workers, mutations and generations, missions, MCP and LSP
configuration, usage charts.
