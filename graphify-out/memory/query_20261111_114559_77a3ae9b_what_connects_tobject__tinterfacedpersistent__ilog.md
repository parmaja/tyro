---
type: "query"
date: "2026-11-11T11:45:59.372049+00:00"
question: "What connects TObject, TInterfacedPersistent, ILog to the rest of the system?"
contributor: "graphify"
outcome: "useful"
source_nodes: ["TObject", "TInterfacedPersistent", "ILog", "TTyroConsoleLog", "TTyroEngine", "TyroApp"]
---

# Q: What connects TObject, TInterfacedPersistent, ILog to the rest of the system?

## Answer

Expanded via vocab: [object, objects, interfaced, persistent, log, mnlogs]; then explain per node. Finding: the three base abstractions are near-isolated in the graph but not in the code. TInterfacedPersistent and ILog (src/TyroEngines.pas L60) have exactly ONE edge each: implemented by TTyroConsoleLog, forming a tiny island (Community 284). The real connector is the global Log pipeline: Log.Install(LogLevel, TTyroConsoleLog.Create) at src/TyroApp.pas L355, then ~24 Log.WriteLn call sites in TyroEngines.pas, raylib/RayClasses.pas, tyrolib/TyroLua.pas, tyrolib/TyroScripts.pas that dispatch through ILog to TTyroEngine.LogWrite - these interface-dispatch edges are not in the graph (Log is a global from an external unit, so AST emits no call edge to the implementation). TObject is fragmented into 16 per-file ghost nodes (e.g. src_tyrolib_tyrosprites_pas_tobject, degree 1: TSprites--inherits-->), one per unit that names it, which is a large part of the 1273 weakly-connected nodes. Bypasses: TTyroEngine.LogWrite(Msg) (L466) is called directly by engine code without ILog, and TTyroTerminal AddLine/Write/Writeln draw console lines directly.

## Outcome

- Signal: useful

## Source Nodes

- TObject
- TInterfacedPersistent
- ILog
- TTyroConsoleLog
- TTyroEngine
- TyroApp