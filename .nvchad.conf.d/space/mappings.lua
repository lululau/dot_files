local M = {}
local SpaceOMappings = require("custom.space.spaceo.mappings")

M.space_j = {
  n = {
    ["<leader>jd"] = { ":Dired<CR>", "Open current directory"},
  }
}

return vim.tbl_deep_extend("force", M, SpaceOMappings)
