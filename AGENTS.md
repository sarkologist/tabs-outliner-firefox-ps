# AGENTS.md

Guidance for AI coding agents (and humans) working in this repo. See
[README.md](README.md) for what the project is and how it's built.

## Development workflow (required)

Follow this for any code-change task:

1. **Worktree when the tree is dirty.** Run `git status` first. If there are
   uncommitted changes (or a long-running process is reading the current
   checkout), do the work in an isolated `git worktree` so it doesn't disturb
   the in-progress state. A clean tree just needs a feature branch.
2. **Always feature branches.** Never commit directly to `master`. Work on a
   dedicated feature branch. When the work depends on another unmerged branch,
   stack on it and set the PR base to that branch (not `master`).
3. **PR when done.** Once the work is complete and the gate is green, open a
   pull request with `gh`.
4. **codex-cli review.** After the PR is up, have codex review it (see below),
   then address the feedback and relay the review to the user.

**Why:** in-progress work stays isolated, changes land as small reviewable units
on branches/PRs, and every change gets an automated second-opinion pass before
it's considered done.

## The one quality gate

`pnpm check` is the single gate, and it must pass before a PR:

```
spago unit/property/guard tests → spago build → esbuild bundle
  → copy-static → web-ext lint → Playwright e2e
```

The project has a strict **no-manual-testing** contract: the whole suite is
deterministic and headless. Add unit + e2e coverage for new behavior rather than
relying on eyeballing the result.

## codex-cli review

- codex-cli is installed at `/opt/homebrew/bin/codex`.
- Run it read-only, scoped to the feature commit(s); tell it to inspect the diff
  and not modify files:

  ```sh
  codex exec --sandbox read-only "Review HEAD — inspect it via 'git show HEAD' \
    and read any files you need; do NOT modify files. <what to scrutinize>"
  ```

- Commits that address a codex finding use the message suffix `(codex review)`
  (e.g. `Harden virtualization (codex review)`). Match it.
