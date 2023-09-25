local M = {}
local SpaceOMappings = require("custom.space.spaceo.mappings")

return vim.tbl_deep_extend("force", M, SpaceOMappings)
