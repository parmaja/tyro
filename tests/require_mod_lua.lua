--======================================================================
--  require_mod_lua.lua - the same module, but with the .lua ending
--  Returns a value so the loader caches it; require("require_mod_lua")
--  and require("require_mod_lua.lua") must both reach this file.
--======================================================================

return { name = "require_mod_lua.lua", answer = 7 }