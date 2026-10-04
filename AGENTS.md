# OpenCode Project Rules

## Compile
- Use FPC that exists in the system, do not use Delphi
- Keep code compatible with Delphi

## Libraries

You can full access current folder
You have full access on minilib source, path is in env %minilib%
You have full access on Lua in path ..\lua\ relative to this current folder

### Lua (LuaAPI) - outside this repo
- The Lua bindings are NOT in this repo. They live in `D:\lab\pascal\lua`, which is
  `..\lua\` relative to the repo root. `LuaAPI.pas`, its prebuilt units and
  `minilua.lpk` (the Lazarus package `tyro.lpi` requires) are all there.
- Any unit that reaches `TyroLua.pas`, directly or through `TyroEngines.pas`, needs
  that folder on its search path or the build dies with:
  `Fatal: (10022) Can't find unit LuaAPI used by TyroLua`
- `OtherUnitFiles` is relative to the folder holding the `.lpi`:
  - `src\tyro.lpi` uses `..\..\lua;raylib;tyrolib`
  - a project in `tests\` needs `..\..\lua`
- Note: `..\src\lua` appears in the older `tests\*.lpi` files but that folder does
  not exist. It is harmless (a missing path is ignored) and is not the Lua location.
- When a build fails on a missing unit, check this outside folder before treating
  it as a code problem.

## Git Commit Guidelines

- Do not commit without asking you to commit
- Do not add ignored files
- Do not add untracked files that you did not generate yourself; leave them untracked
- Whenever you are asked to make a git commit, you must identify and include your active model name inside the commit message.

## Testing Framework Architecture

- Leave test files after finish
- All test files must be stored strictly within the `tests/` directory at the root of the repository. 
- Do not create or co-locate test files inside the application source directories.
