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
4. **Independent AI review.** After the PR is up, have a different agent review
   it (see below), then address the feedback and relay the review to the user.

**Why:** in-progress work stays isolated, changes land as small reviewable units
on branches/PRs, and every change gets an automated second-opinion pass before
it's considered done.

## The one quality gate

`pnpm check` is the single gate, and it must pass before a PR:

```
dev-path check → spago unit/property/guard tests → spago build
  → esbuild bundle → copy-static → web-ext lint → Playwright e2e
```

GitHub Actions runs the same `pnpm check` on every PR and push to `master`
(`.github/workflows/ci.yml`); `master` requires it to pass. The dev-path check
(`scripts/check-dev-paths.mjs`) fails on absolute developer home/toolchain paths
in tracked files, since the AMO source zip ships every committed file to
Mozilla's reviewers.

The project has a strict **no-manual-testing** contract: the whole suite is
deterministic and headless. Add unit + e2e coverage for new behavior rather than
relying on eyeballing the result.

## Independent AI review

- The reviewer must be an independent agent: a fresh session with no context
  from the implementation, given the diff rather than the author's conclusions.
  Which tool runs it does not matter — e.g. Claude Code (`claude` on PATH) or
  codex-cli (`codex`), whichever is available.
- Run it read-only, scoped to the feature commit(s); tell it to inspect the
  diff and not modify files:

  ```sh
  claude -p --permission-mode plan "Review HEAD — inspect it via 'git show HEAD' \
    and read any files you need; do NOT modify files. <what to scrutinize>"
  # or
  codex exec --sandbox read-only "Review HEAD — inspect it via 'git show HEAD' \
    and read any files you need; do NOT modify files. <what to scrutinize>"
  ```

- Commits that address review feedback use the suffix `(ai review)`.
