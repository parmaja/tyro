# CI

[<< Tyro docs index](README.md)

`.github/workflows/build.yml` builds Tyro on a Windows runner with FPC 3.3.1
(the trunk snapshot) and, when the runtime DLLs are available, runs the
15-case smoke suite from
[`tests/run_roadmap02.ps1`](../tests/run_roadmap02.ps1).

## What the workflow does

1. Checks out `tyro` plus the two sibling packages it compiles against:
   `minilib` (`..\minilib`) and `miniLua` (`..\lua`).
2. Installs stable Lazarus with `gcarreno/setup-lazarus@v3` for `lazbuild` and
   the LCL packages. FPC 3.3.x is not bundled with any stable Lazarus, so the
   workflow downloads the official daily win64 snapshot
   (`fpc-3.3.1.x86_64-win64.built.on.i386-win32.zip`) from
   `downloads.freepascal.org/fpc/snapshot/trunk/x86_64-win64/`, generates an
   `fpc.cfg` for its prebuilt units, and builds with
   `lazbuild --compiler=<snapshot>/bin/i386-win32/ppcrossx64.exe --build-all --build-mode=Debug tyro.lpi`
   in `src/`.
3. Downloads the win64 runtime DLLs into `bin/` and runs the smoke suite.

The trunk snapshot is replaced daily, so CI tracks the current FPC 3.3.1
development state rather than a frozen compiler build.

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

With a local FPC 3.3.x already on `PATH`, `lazbuild` picks it up automatically:

```
cd src
lazbuild --build-all --build-mode=Debug tyro.lpi
cd ..
powershell -ExecutionPolicy Bypass -File tests\run_roadmap02.ps1
```

If Lazarus is configured for a different compiler, point it at FPC 3.3
explicitly (the binary is `ppcx64.exe` in a native install and
`ppcrossx64.exe` in the snapshot zip):

```
lazbuild --compiler=<path-to-fpc-3.3>\bin\x86_64-win64\ppcx64.exe --build-all --build-mode=Debug tyro.lpi
```

See [`tests/README.md`](../tests/README.md) for how to add a case to the suite.
Use the installed FPC/Lazarus toolchain — not Delphi — as required by
`AGENTS.md`.
