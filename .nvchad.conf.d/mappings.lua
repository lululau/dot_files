---@type MappingsTable
local M = {}

local SpaceKeyMappings = require("custom.space.mappings")

M.general = {
  v = {
    [">"] = { ">gv", "indent"},
    ["<C-z>"] = {"<esc>", "Escape to normal mode"}
  },

  n = {
    ["<Esc>"] = { ":noh <CR>", "Clear highlights" },
    ["<A-h>"] = { "<C-w>h", "Window left" },
    ["<A-l>"] = { "<C-w>l", "Window right" },
    ["<A-j>"] = { "<C-w>j", "Window down" },
    ["<A-k>"] = { "<C-w>k", "Window up" },

    ["<leader>tn"] = { "<cmd> set nu! <CR>", "Toggle line number" },
    ["<leader>bn"] = { "<cmd> enew <CR>", "New buffer" },
    ["<leader>hc"] = { "<cmd> NvCheatsheet <CR>", "Mapping cheatsheet" },
    ["<leader>qq"] = { "<ESC>:qa<CR>", "Force quit all" },
    ["<leader>bd"] = { "<ESC>:bd!<CR>", "Close current buffer" },
    ["<leader>bh"] = { ":Nvdash<CR>", "Open Dashboard" },

    ["<leader>'"] = { function()
        require("nvterm.terminal").toggle "horizontal"
     end,
     "Open terminal"
    },

    ["<D-'>"] = { function()
        require("nvterm.terminal").toggle "horizontal"
     end,
     "Open terminal"
    },

    ["<leader>="] = {
      function()
        vim.lsp.buf.format { async = true }
      end,
      "LSP formatting",
    },
    ["<D-j>"] = { "10j", "Move down 10 lines" },
    ["<C-x><C-s>"] = {"<ESC>:w<CR>", "Save"},
    ["<c-x><c-z>"] = {"<C-z>", "Let vim go background"},
    ["<C-z>"] = {"i", "Enter insert mode"},
    ["<C-a>"] = { "^", "Move to first character"},
    ["<D-t>"] = { ":belowright vsplit | wincmd w<CR>", "Split window vertically", opts = {silent = true}},
    ["<D-w>"] = { ":close<CR>", "Close window", opts = {silent = true}},
    ["<C-CR>"] = {
      function()
        vim.lsp.buf.definition()
      end,
      "LSP definition"
    },
    ["<D-CR>"] = {
      function()
        vim.lsp.buf.declaration()
      end,
      "LSP declaration"
    },
    ["<S-CR>"] = {
      function()
        vim.lsp.buf.references()
      end,
      "LSP references"
    },
    ["<c-x><c-o><c-e>"] = {
      function()
        vim.lsp.buf.definition()
      end,
      "LSP definition"
    },
    ["<c-x><c-o><c-a>"] = {
      function()
        vim.lsp.buf.declaration()
      end,
      "LSP declaration"
    },
    ["<c-x><c-o><c-b>"] = {
      function()
        vim.lsp.buf.references()
      end,
      "LSP references"
    },
  },

  i = {
    ["<D-j>"] = { "<C-o>10j", "Move down 10 lines" },
    ["<D-k>"] = { "<C-o>10k", "Move up 10 lines" },
    ["<C-x><C-s>"] = {"<C-o>:w<CR>", "Save"},
    ["<C-n>"] = {"<C-j>", "Move to next line"},
    ["<C-p>"] = {"<C-k>", "Move to previous line"},
    ["<C-z>"] = {"<esc>", "Escape to normal mode"},
    ["<C-a>"] = { "<C-o>^", "Move to first character"},
    ["<C-e>"] = { "<C-o>A", "Move to end of line"},
    ["<C-f>"] = { "<Right>", "Move to right"},
    ["<C-b>"] = { "<Left>", "Move to left"},
    ["<C-d>"] = { "<Del>", "Delete character"},
    ["<A-f>"] = { "<C-o>e", "Move to next word"},
    ["<A-b>"] = { "<C-o>b", "Move to previous word"},
    ["<A-<>"] = { "<C-o>gg<C-o>0", "Move to first line"},
    ["<A->>"] = { "<C-o>G<C-o>$", "Move to last line"},
    ["<A-bs>"] = { "<ESC><ESC>caw", "Delete word"},
    ["<A-tab>"] = { "<ESC>:b#<CR>", "Switch to previous buffer"},
    ["<D-t>"] = { "<C-o>:belowright vsplit | wincmd w<CR>", "Split window vertically"},
    ["<D-w>"] = { "<C-o>:close<CR>", "Close window"},
    ["<C-CR>"] = {
      function()
        vim.lsp.buf.definition()
      end,
      "LSP definition"
    },
    ["<D-CR>"] = {
      function()
        vim.lsp.buf.declaration()
      end,
      "LSP declaration"
    },
    ["<S-CR>"] = {
      function()
        vim.lsp.buf.references()
      end,
      "LSP references"
    },
    ["<c-x><c-o><c-e>"] = {
      function()
        vim.lsp.buf.definition()
      end,
      "LSP definition"
    },
    ["<c-x><c-o><c-a>"] = {
      function()
        vim.lsp.buf.declaration()
      end,
      "LSP declaration"
    },
    ["<c-x><c-o><c-b>"] = {
      function()
        vim.lsp.buf.references()
      end,
      "LSP references"
    },
  },

  c = {
    ["<C-n>"] = {"<C-j>", "Move to next line"},
    ["<C-p>"] = {"<C-k>", "Move to previous line"},
    ["<A-f>"] = {"<S-Right>", "Move to right"},
    ["<A-b>"] = {"<S-Left>", "Move to left"},
    ["<A-bs>"] = {"cw", "Delete word"},
    ["<C-b>"] = {"<Left>", "Move to left"},
    ["<C-f>"] = {"<Right>", "Move to right"},
    ["<C-a>"] = {"<C-b>", "Move to left"},
    ["<C-p>"] = {"<Up>", "Move to previous line"},
    ["<C-n>"] = {"<Down>", "Move to next line"}
  },


  s = {
    ["<C-x><C-s>"] = {"<C-o>:w<CR>", "Save"},
  },

  t = {
    ["<D-'>"] = { function()
        require("nvterm.terminal").toggle "horizontal"
     end,
     "Open terminal"
    },
  }
}

M.general[{"n", "v", "i"}] = {
    ["<D-j>"] = { "10j", "Move down 10 lines" },
    ["<D-k>"] = { "10k", "Move up 10 lines" },
    ["<F1>"] = { ":NvimTreeToggle<CR>", "Toggle file explorer" },
    ["<C-c>"] = { "<ESC>", "ESC"},
    ["<C-x><C-x>"] = { "<ESC>:qa!<CR>", "Force quit all"},
    ["<C-x><C-s>"] = { "<ESC>:w<CR>", "Save"},
    ["<C-x><C-k>"] = { "<ESC>:bw!<CR>", "Close buffer"},
    ["<A-tab>"] = { ":b#<CR>", "Switch to previous buffer"},
    ["<A-w>"] = { "<C-w>", "Move to previous window"},
    ["<C-x><C-c>"] = { "<ESC>:qa<CR>", "Quit all"},
    ["<A-h>"] = {"<C-w>h", "Move to left window"},
    ["<A-j>"] = {"<C-w>j", "Move to down window"},
    ["<A-k>"] = {"<C-w>k", "Move to up window"},
    ["<A-l>"] = {"<C-w>l", "Move to right window"},
}

M.copy_paste = {
  n = {
    ["<D-s>"] = { ":w<CR>", "save", opts = { nowait = true} },
    ["<D-c><D-c>"] = { '"+yy', "copy line", opts = { nowait = true} },
    ["<D-c>p"] = { '"+yip', "copy paragraph", opts = { nowait = true} },
    ["<D-v>"] = { '"+P', "paste normal mode", opts = { nowait = true} }
  },

  v = {
    ["<D-c>"] = { '"+y', "copy", opts = { nowait = true} },
    ["<D-v>"] = { '"+P', "paste visual mode", opts = { nowait = true} }
  },

  c = {
    ["<D-v>"] = { "<C-R>+", "paste command mode", opts = { nowait = true} }
  },

  i = {
    ["<D-v>"] = { "<C-o>\"+P", "paste insert mode", opts = { nowait = true} }
  }
}

M.telescope = {
  plugin = true,

  n = {
    -- find
    ["<leader>ff"] = { "<cmd> Telescope find_files <CR>", "Find files" },
    ["<D-f>"] = { "<cmd> Telescope find_files <CR>", "Find files" },
    ["<leader>fa"] = { "<cmd> Telescope find_files follow=true no_ignore=true hidden=true <CR>", "Find all" },
    ["<D-o>"] = { "<cmd> Telescope find_files follow=true no_ignore=true hidden=true <CR>", "Find all" },
    ["<leader>/"] = { "<cmd> Telescope live_grep <CR>", "Live grep" },
    ["<leader>bb"] = { "<cmd> Telescope buffers <CR>", "Find buffers" },
    ["<D-b>"] = { "<cmd> Telescope buffers <CR>", "Find buffers" },
    ["<leader>fh"] = { "<cmd> Telescope help_tags <CR>", "Help page" },
    ["<leader>fr"] = { "<cmd> Telescope oldfiles <CR>", "Find oldfiles" },
    ["<leader>ss"] = { "<cmd> Telescope current_buffer_fuzzy_find <CR>", "Find in current buffer" },

    -- git
    ["<leader>gl"] = { "<cmd> Telescope git_commits <CR>", "Git commits" },
    ["<leader>gs"] = { "<cmd> Telescope git_status <CR>", "Git status" },
    ["<D-g>"] = { "<cmd> Telescope git_status <CR>", "Git status" },

    -- pick a hidden term
    ["<leader>pt"] = { "<cmd> Telescope terms <CR>", "Pick hidden term" },

    -- theme switcher
    ["<leader>th"] = { "<cmd> Telescope themes <CR>", "Nvchad themes" },

    ["<leader>fb"] = { "<cmd> Telescope marks <CR>", "telescope bookmarks" },
    ["<D-i><D-b>"] = { "<cmd> Telescope marks <CR>", "telescope bookmarks" },
  },
}


return vim.tbl_deep_extend("force", M, SpaceKeyMappings)