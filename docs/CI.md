# CI

[<< Tyro docs index](README.md)

`.github/workflows/build.yml` builds Tyro on a Windows runner with FPC/Lazarus
and, when the runtime DLLs are available, runs the 15-case smoke suite from
[`tests/run_roadmap02.ps1`](../tests/run_roadmap02.ps1).

## What the workflow does

1. Checks out `tyro` plus the two sibling packages it compiles against:
   `minilib` (`..\minilib`) and `miniLua` (`..\lua`).
2. Installs stable Lazarus with `gcarreno/setup-lazarus@v3` and runs
   `lazbuild --build-all --build-mode=Debug tyro.lpi` in `src/`.
3. Downloads the win64 runtime DLLs into `bin/` and runs the smoke suite.

## Repository variables

The DLLs are intentionally not committed (`.gitignore` ignores `*.dll`), so
the smoke step needs two repository variables:

| Variable | Value |
|----------|-------|
| `RAYLIB_URL` | URL of a zip containing `raylib-6.2-64.dll` (or the version you build against) |
| `CHIPMUNK_URL` | Optional URL of a zip containing `chipmunk.dll` |

Set them under **Settings → Secrets and variables → Actions → Variables**.
Without `RAYLIB_URL` the build still runs; the smoke step is skipped because
the game cannot launch without raylib.

## Local equivalent

```
cd src
lazbuild --build-all --build-mode=Debug tyro.lpi
cd ..
powershell -ExecutionPolicy Bypass -File tests\run_roadmap02.ps1
```

See [`tests/README.md`](../tests/README.md) for how to add a case to the suite.
Use the installed FPC/Lazarus toolchain — not Delphi — as required by
`AGENTS.md`.
