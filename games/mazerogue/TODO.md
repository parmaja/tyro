## Add features

* [x] Enemy cant see through walls so not move toward the player or shoot if player behind wall
* [x] Enemy should not overlapped each others

* [x] A Key is hidden under one of crates (carts)
* [x] Levelup door not open without key

* [x] New enemy: immobile Plasma Turret cannon, only destroyed by bombs (grenades/barrels), gun rounds ricochet
* [x] Enemies remember player about 3 seconds after he is out of sight and keep chasing/attacking the last seen spot

* [x] Sector darkness from B-2 using a flashlight shader (torch ring around player, radius configured in D.LIGHT_RADIUS)
* [x] Remove the mini-map (radar) from sector B-3 and deeper
* [x] Fix light not following player at maze bottom (area uniform was double-flipped, engine already flips it)
* [x] Loading splash with world.enable = false like Jet2026 (boot + sector transition); fix colors.cyan -> colors.aqua (cyan is not a registered engine color)

