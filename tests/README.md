# Tests

Most files in this folder are ad-hoc scripts and recorded `_out.txt` goldens
kept locally during development. They are intentionally ignored by Git (see
`.gitignore`); only this README and `run_roadmap02.ps1` are committed.

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

## Adding a test

Put scripts and goldens **only** in this folder, as required by `AGENTS.md`.
Large engine-level checks use Lazarus `.lpr` programs here too (for example
`test_editor_highlight.lpr`, `test_touch_frame.lpr`). To make a new check part
of the routine suite, add a case to `run_roadmap02.ps1` and commit that change
alongside the script if the check belongs to roadmapped work.
