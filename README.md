# agent-shell-swarm

A dashboard for controlling multiple running [agent-shell](https://github.com/xenodium/agent-shell)
instances from one buffer.

`M-x agent-shell-swarm` lists every agent-shell buffer with live status
and lets you dispatch prompts, answer permission requests, interrupt,
fork, and kill agents — without visiting their buffers.

```
 Claude Agent @ backend        busy     Claude     Fable 5          Accept Edits   75%  USD1.42  35s ~/code/backend/    Fix the flaky test
 Claude Agent @ frontend       blocked  Claude     Fable 5          Plan            2%  USD0.30   2m ~/code/frontend/   Add login form validation
 Pi Agent @ tether             ready    Pi         GPT-5.6 Sol      Thinking: med…       USD0.85   1h ~/vetted/tether/   Implement the MVP
```

The mode line sums up the swarm: `4 agents · 2 busy · 1 blocked · USD9.41`.

## Features

- **Live columns** per agent: status (`ready`/`busy`/`blocked`/`dead`),
  agent, model, session mode, context usage %, accumulated cost, time
  since last activity, working directory, and session title.
- **Event-driven refresh** — the dashboard subscribes to each shell's
  ACP events and updates itself when anything changes (debounced); no
  polling, no manual refreshing.
- **Detail view** — pending permission request (what the agent wants to
  do, with its raw input), answerable in place; tool-call history; and
  the files the agent has written, as clickable links.
- **Bulk operations** — mark agents, then interrupt or kill them all
  with one confirmation. Marks survive refreshes.
- **Triage** — jump straight to the next agent waiting on a permission.
- **evil-mode support** out of the box.

## Requirements

- Emacs 29.1+
- [agent-shell](https://github.com/xenodium/agent-shell) (and its
  dependencies: shell-maker, acp)
- [evil](https://github.com/emacs-evil/evil) is optional; bindings are
  added when it's loaded

## Installation

Not on (M)ELPA. With Emacs 29+ built-in `package-vc`:

```elisp
(package-vc-install "https://github.com/Endi1/agent-shell-swarm")
```

or with `use-package`:

```elisp
(use-package agent-shell-swarm
  :vc (:url "https://github.com/Endi1/agent-shell-swarm")
  :commands (agent-shell-swarm))
```

## Usage

`M-x agent-shell-swarm` opens the dashboard.

| Key   | Action                                                     |
|-------|------------------------------------------------------------|
| `RET` | visit the agent's shell buffer                             |
| `n`/`p` | move to the next / previous agent (Dired-style)          |
| `+`   | start a new agent (prompts for directory and agent)        |
| `s`   | send a prompt to the agent at point (stays in dashboard)   |
| `i`   | interrupt the agent at point                               |
| `f`   | fork the agent: new shell, same conversation               |
| `b`   | jump to the next blocked agent                             |
| `v`   | inspect: permission request, tool calls, changed files     |
| `m`/`u`/`U` | mark / unmark / unmark all                           |
| `d`   | mark the agent for killing (like Dired)                    |
| `x`/`K` | kill all marked agents                                  |
| `D`   | kill the agent at point immediately                        |
| `I`   | interrupt all marked agents                                |
| `g`   | refresh manually                                           |

In the detail view (`v`): `a` answers the pending permission request
(choosing among the options the agent offered), `g` refreshes, `q`
quits. Changed-file entries are buttons that open the file.

With evil-mode, the same Dired-like keys work in normal/motion state;
refresh is on `gr`.

### Notes

- Sending to a busy or blocked agent asks for confirmation first.
- Changed-files tracking records ACP filesystem writes and completed
  file-modifying tool calls (edit/delete/move) while a dashboard has
  been open; files created as side effects of shell commands the agent
  runs are not seen.
- Fork requires the agent to advertise session-fork support.

## Development

```sh
make check   # byte-compile (warnings as errors) + run the ERT suite
```

Tests live in `tests/agent-shell-swarm-test.el` and run against a fake
shell fixture; no real agents are started.
