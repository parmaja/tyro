# AI Agents Project Rules

This is a simple game engine for kids and noobs, using RayLib and Lua for endusers, built using FPC/Delphi

## Implementation

* Big task split it to subtasks, for not lose the whole quota on one big task, and can continue with next task next day if ranout of quota
* Read documents in `help` folder
* Do not use pcall in lua, most functions are queue to the engine
* Try to update the documents (README/HELP etc) instead to update the code based on document
* It is new project, so do not make legacy compatible, fix the demo/games/code if necessary

### Games

* If the task is a game, use Sprites as possible, add any features that needed it in Tyro project, make it smooth and perfect game
* Use `update(dt)` and `draw()` functions callback; `cycle` is removed/legacy
* Do not forget to make splash loading screen
* Use for creating assets lua/tyro from game not python, as possible, add the feature in Tyro Engine if needed

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
- Whenever you are asked to make a git commit, you must identify and include your active model name at end of comment and as the git author but do not change email in git.

## Testing Framework Architecture

- Leave test files after finishing.
- All test files must be stored strictly within the `tests/` directory at the root of the repository.
- Do not create or co-locate test files inside the application source directories.