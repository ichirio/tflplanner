# Agent / contributor instructions for tflplanner

This repository **is the R package** `tflplanner` (`DESCRIPTION` is at the
root): a 'shiny' app and functions that manage clinical TFL studies on top of
[tflspec](https://github.com/ichirio/tflspec) and
[rtfreporter](https://github.com/ichirio/rtfreporter).  This file only points
to the documents that hold the rules.

## Read these first

| Topic | Document |
|-------|----------|
| The rules for working on the code (dependencies, screens, PRs, tests, files) | `CLAUDE.md` |
| How to contribute: issues, fork and pull request, the checks before a PR | `CONTRIBUTING.md` |
| What the app is made of (studies, specs, screens) | `vignettes/articles/concepts.Rmd` |
| The AI tab's flow | `vignettes/articles/ai-flow.Rmd` |

`CLAUDE.md` is written for any coding agent, not only Claude: read it as this
repository's agent guide.  Where this file and those documents disagree,
they win.

## In short

- Dependencies point one way: rtfreporter <- tflspec <- tflplanner.  A change
  to the table layout belongs in rtfreporter, a change to the spec in tflspec.
- R code is ASCII (`\uXXXX` escapes).  Every text the app shows has its
  Japanese in `inst/i18n/strings/<file>.csv`.
- **Record the change in the PR description.**  A PR never edits `NEWS.md`,
  `CHANGELOG.md` or `Version:`, and there is no separate memo file.  Fill the
  template's *User-visible change (for NEWS)* field without being asked: one or
  two bullets in NEWS style (what a user can now do, or what is fixed, named by
  its function or argument), or "none" for an internal change; then "Breaking:
  no", or "Breaking: yes" with the migration.  The maintainers write `NEWS.md`
  from the merged PRs at release.  For example:

  ```markdown
  ## User-visible change (for NEWS)

  - The AI tab's docs field adds a file at once: on Browse, on Enter, or on
    leaving the field.

  Breaking: no
  ```
- Before a pull request: the tests of what you changed and `lintr` on the
  changed files (see `CONTRIBUTING.md`).
- Outside contributors open pull requests against `main`.  The `next` branch,
  the `automerge` label and `merge-after:` lines are the maintainers' own
  workflow; a maintainer adds any label that is needed.

## Git work for a contributor

If you are an agent working for a contributor, you do the git work as well.
Their fork is `origin`; `https://github.com/ichirio/tflplanner.git` is `upstream`
(`git remote add upstream <url>`, one time only). Check with `git remote -v`
before you start. Run `git status` first; if the working tree has uncommitted
changes, commit them or ask, and never discard them.

### Update the fork's main

Do this before every new piece of work. A fork does not update itself. Keep
`main` pristine: never commit to it.

```bash
git checkout main
git fetch upstream
git merge --ff-only upstream/main
git push origin main
```

Then cut the topic branch: `git checkout -b <type>/<issue>-<slug>`.

### If the fork's main has gone stale or has its own commits

If `git merge --ff-only` fails, `main` has commits upstream does not have.
Stop and tell the contributor which commits (`git log upstream/main..main`).
The reset below discards them, so run it only after they agree, and keep the
work on a branch first:

```bash
git branch my-old-work main
git fetch upstream
git checkout main
git reset --hard upstream/main
git push origin main --force-with-lease
```

This is the only time `main` is ever force-pushed.

### Rebase a topic branch on upstream/main

Do this when the branch has fallen behind, or when CI or a reviewer asks for it.

```bash
git fetch upstream
git checkout <topic-branch>
git rebase upstream/main
```

If there are conflicts, git stops at each one:

1. `git status` lists the conflicted files. Open each one and keep both
   sides' intent: upstream's change and the contributor's.
2. If you cannot tell what a side intended, read its commit
   (`git log -p upstream/main -- <file>`). If you still cannot tell, run
   `git rebase --abort` and ask. Don't guess.
3. `git add <file>`, then `git rebase --continue`. Repeat until it finishes.
   Never edit `Version:` or `NEWS.md` to settle a conflict; take upstream's.

Then run the tests of the area you changed again, because a rebase changes the
code under them. Only when they pass:

```bash
git push --force-with-lease origin <topic-branch>
```

- `--force-with-lease` on the topic branch only: a rebase rewrites its history,
  so a plain push is refused.
- Never use plain `--force`. If `--force-with-lease` is refused, someone else
  pushed to the branch: fetch and look at what they pushed before going on.
- Never force-push `main`, except for the stale-fork reset above.

### When CI or a review asks for a rebase

- "This branch is out of date" or "has conflicts": rebase as above and push. The
  pull request updates by itself; don't open a new one, and don't close it.
- CI failed: read the failed step's log first. If the failure comes from your
  change, fix it in a new commit on the branch. Rebase only if upstream
  has moved past a fix you need.
- A review asked for changes: add commits on the branch and reply on the pull
  request. Don't rewrite history that the reviewer has already read, unless
  they asked for a rebase or squash.
- Stop and ask the contributor if a rebase would drop commits you didn't
  write, or if conflicts reach beyond the files they changed.
