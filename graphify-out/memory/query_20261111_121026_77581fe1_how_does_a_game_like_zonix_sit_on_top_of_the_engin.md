---
type: "query"
date: "2026-11-11T12:10:26.021466+00:00"
question: "How does a game like zonix sit on top of the engine stack (exploration)?"
contributor: "graphify"
outcome: "useful"
source_nodes: ["zonix/make_assets.py", "ZONIX - Cyber Arcade Territorial Conquest", "Breadth-first flood-fill territory claiming", "tests/test_zonix_data.tyro smoke test and attract-mode screenshot test"]
---

# Q: How does a game like zonix sit on top of the engine stack (exploration)?

## Answer

Expanded via vocab: [zonix]. Finding 1: every game shares an asset-factory pattern - games/<name>/make_assets.py (Pillow + wave) generates PNG strips AND .sprite zip atlases plus sfx at build time; the semantic layer cross-linked these (semantically_similar_to across zonix/submarine/seaquest/neonrunner/breakout/... and a hyperedge 'Self-contained procedurally generated game assets'). Finding 2: games' knowledge enters via README.md concept nodes (e.g. games_zonix_readme_territory_flood_fill, orb_glow.frag shader, power-ups, tests/test_zonix_data smoke test) and image-asset nodes with frame descriptions. BLIND SPOT (verified): the actual game scripts are .tyro (27 files in games/) and .tyro is NOT in graphify CODE_EXTENSIONS; all .lua files live under tests/ which .gitignore excludes (/tests) - so 0 script nodes from the user-facing game layer exist; only doc/asset-level coverage. Fix options: map .tyro as lua dialect (upstream request or symlink/copy sidecars) and/or un-ignore tests for graphify detection.

## Outcome

- Signal: useful

## Source Nodes

- zonix/make_assets.py
- ZONIX - Cyber Arcade Territorial Conquest
- Breadth-first flood-fill territory claiming
- tests/test_zonix_data.tyro smoke test and attract-mode screenshot test