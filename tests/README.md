# Tests

Most files in this folder are ad-hoc scripts kept locally during development
and ignored by Git (see `.gitignore`). The committed files are this README,
`run_roadmap02.ps1`, the smoke scripts it runs and their goldens — a fresh
checkout can run the suite immediately.

## Run the roadmap smoke suite

From the repository root, after building `bin/tyro.exe` with FPC/Lazarus:

```
powershell -ExecutionPolicy Bypass -File tests\run_roadmap02.ps1
```

The runner executes 15 scripts with `--exit`/`--exit:N` and checks each exit
code **plus** a golden file or a stdout pattern for most cases — so a script
that silently stops early cannot pass on exit code alone. A failing script is
expected to exit 1 (for example
`syntax_error_audit.tyro` and the watchdog test); everything else must exit 0.
It also prints the command for the in-window error-banner screenshot:

```
bin\tyro.exe tests\roadmap02_p0_banner_test.tyro --exit:2 --screenshot:1
```

A `PASS`/`FAIL` line is printed per case, and the script itself exits 1 if any
case failed. All 15 pass after the ROADMAP02 changes.

## Rebuild first

```
cd src
lazbuild --build-all --build-mode=Debug tyro.lpi
```

Use the FPC/Lazarus toolchain already installed on the machine — not Delphi.

## Game test scripts

Game-data and game-play harnesses were retired with the `tests/` cleanup.
This folder now holds engine tests only; game rules are checked through the
engine tests here, and a game can still be launched directly from its own
folder with `--workpath` and `--screenshot`.

## Adding a test

Put scripts and goldens **only** in this folder, as required by `AGENTS.md`.
Large engine-level checks use Lazarus `.lpr` programs here too (for example
`test_editor_highlight.lpr`, `test_touch_frame.lpr`). To make a new check part
of the routine suite, add a case to `run_roadmap02.ps1` and commit that change
alongside the script if the check belongs to roadmapped work.

## Cleanup policy

This is a new project: no legacy scripts are kept. A script that uses `cycle`,
a retired `controls.*` property spelling, or a removed button spelling on
`game.*` is either fixed to the current API or deleted. Generated artifacts
(binaries, logs, screenshots, probe outputs) are not kept; the smoke runner
regenerates the few probe files it needs. Scripts that intentionally contain
an absolute path (the `openfile` sandbox tests that must reject `C:\Windows`)
are the exception, not legacy.
