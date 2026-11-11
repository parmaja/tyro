---
type: "query"
date: "2026-11-11T11:49:55.489856+00:00"
question: "How does TyroScripts connect Lua scripts to the engine subsystems?"
contributor: "graphify"
outcome: "useful"
source_nodes: ["TyroScripts", "TyroLua", "TTyroScriptThread", "TQueueObject", "RunChunk", "TLuaSprites"]
---

# Q: How does TyroScripts connect Lua scripts to the engine subsystems?

## Answer

Expanded via vocab: [tyro, scripts, lua, engine, sprites, sounds, physics, canvas, models, commands, console, execute]; traced with explain + path. Two bridge layers: TyroScripts (src/tyrolib/TyroScripts.pas, Community 73, degree 107) is the command-object interpreter: it contains ~70 T<Verb>Object command classes (TPlaySoundObject, TDrawCircleObject, TSetControl*Object, TShowConsoleObject...) each clustered alone in its own micro-community (310-337), plus the execution core TQueueObject (L20), ExecuteQueueObject/RunQueueObject, TTyroScriptThread (L679); imports the engine spine (TyroClasses, TyroControls, TyroSounds, TyroSpectrum, TyroTerminal, Melodies, raylib) and TyroEngines.pas at L804. TyroLua (Community 66, degree 100) is the Lua binding layer: registers TLua* wrappers (TLuaSprites/TLuaCanvas/TLuaSound/TLuaWorld/TLuaGamepad...) and runs scripts via RunChunk(); imports 18 engine units. The two meet directly: TyroLua --imports--> TyroScripts.pas (2-hop path, EXTRACTED) and TyroLua itself defines command objects (TRadioPlayObject, TShowSpectrumObject) - Lua commands reuse the same command-queue substrate. Touches 42 communities. Missing edges: DoExecute bodies dispatch to engine functions without resolved per-symbol call edges, so command classes appear as leaves.

## Outcome

- Signal: useful

## Source Nodes

- TyroScripts
- TyroLua
- TTyroScriptThread
- TQueueObject
- RunChunk
- TLuaSprites