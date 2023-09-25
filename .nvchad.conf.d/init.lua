-- local autocmd = vim.api.nvim_create_autocmd

-- Auto resize panes when resizing nvim window
-- autocmd("VimResized", {
--   pattern = "*",
--   command = "tabdo wincmd =",
-- })

vim.o.ignorecase = true
vim.o.smartcase = true

-- gui font
vim.o.guifont = "JetBrainsMono\\ Nerd\\ Font:h13"

if vim.g.neovide then
    vim.g.neovide_transparency = 0.0
    vim.g.transparency = 1.0
    vim.g.neovide_background_color = string.format("#272c36%x", math.floor(255 * vim.g.transparency))
    vim.g.neovide_input_macos_alt_is_meta = true
end

if vim.g.started_by_firenvim then
  vim.api.nvim_set_keymap('i', '«', '<Plug>(copilot-suggest)', {noremap = true, silent = true})
  vim.api.nvim_set_keymap('i', '‘', '<Plug>(copilot-next)', {noremap = true, silent = true})
  vim.api.nvim_set_keymap('i', '“', '<Plug>(copilot-previous)', {noremap = true, silent = true})

  vim.g.firenvim_config = {
    globalSettings = { alt = "all" },
    localSettings = {
      [".*"] = {
        cmdline  = "neovim",
        content  = "text",
        priority = 0,
        selector = "nothing",
        takeover = "always"
      },

      ["https://jenkins."] = {
        cmdline  = "neovim",
        content  = "text",
        priority = 99,
        selector = "textarea",
        takeover = "always"
      }
    }

  }

  local handle = io.popen("system_profiler SPDisplaysDataType | grep -E '5120|Retina'")
  local result = handle:read("*a")
  handle:close()
  if result ~= "" then
    vim.cmd "set guifont=JetBrainsMono\\ Nerd\\ Font:h18"
  else
    vim.cmd "set guifont=JetBrainsMono\\ Nerd\\ Font:h11"
  end
end

require "custom.misc"
require "custom.space"
