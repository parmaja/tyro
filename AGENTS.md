# AI Agents Project Rules

This is a simple game engine for kids and noobs, using RayLib and Lua for endusers, built using FPC/Delphi

## Implementation

* Big task split it to subtasks, for not lose the whole quota on one big task, and can continue with next task next day if runout of quota
* Read documents in `help` folder

### Games

* If the task is a game, use Sprites as possible, add any features that needed it in Tyro project, make it smooth and perfect game
* Use `update(dt)` and `draw()` functions callback; `cycle` is removed/legacy

## Compile

- Use FPC that exists in the system, do not use Delphi
- Keep code compatible with Delphi

## Libraries

You have full access to the current folder and this relative paths.

- `.\`
- `..\minilib`
- `..\lua\`

## Git Commit Guidelines

- Do not commit without being asked to commit.
- Do not add ignored files.
- Do not add untracked files that you did not generate yourself; leave them untracked.
- Whenever you are asked to make a git commit, you must identify and include your active model name at top of comment and as the author but do not change email in git.

## Testing Framework Architecture

- Leave test files after finishing.
- All test files must be stored strictly within the `tests/` directory at the root of the repository.
- Do not create or co-locate test files inside the application source directories.