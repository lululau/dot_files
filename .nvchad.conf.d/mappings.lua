---@type MappingsTable
local M = {}

local SpaceKeyMappings = require("custom.space.mappings")

M.general = {
  v = {
    ["<D-q>"] = { "<ESC>", "Enter normal mode"},
    [">"] = { ">gv", "indent"},
    ["<C-z>"] = {"<esc>", "Escape to normal mode"}
  },

  n = {
    ["<D-q>"] = { "i", "Enter insert mode"},
    ["<Esc>"] = { ":noh <CR>", "Clear highlights" },
    ["<A-h>"] = { "<C-w>h", "Window left" },
    ["<A-l>"] = { "<C-w>l", "Window right" },
    ["<A-j>"] = { "<C-w>j", "Window down" },
    ["<A-k>"] = { "<C-w>k", "Window up" },

    ["<leader>tn"] = { "<cmd> set nu! <CR>", "Toggle line number" },
    ["<leader>bn"] = { "<cmd> enew <CR>", "New buffer" },
    ["<leader>hc"] = { "<cmd> NvCheatsheet <CR>", "Mapping cheatsheet" },
    ["<leader>qq"] = { "<ESC>:qa!<CR>", "Force quit all" },
    ["<leader>bd"] = { "<ESC>:bd!<CR>", "Close current buffer" },
    ["<leader>bh"] = { "<cmd> Nvdash<CR>", "Open Dashboard" },
    ["<leader>en"] = { "<cmd> lua vim.diagnostic.goto_next()<CR>", "Next Error" },
    ["<leader>ep"] = { "<cmd> lua vim.diagnostic.goto_prev()<CR>", "previous Error" },
    ["]e"] = { "<cmd> lua vim.diagnostic.goto_next()<CR>", "Next Error" },
    ["[e"] = { "<cmd> lua vim.diagnostic.goto_prev()<CR>", "previous Error" },
    ["<leader>el"] = { "<cmd> Telescope diagnostics<CR>", "All Errors" },

    ["<leader>fW"] = { "<cmd> w !sudo tee % > /dev/null<CR>", "Write as root" },

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


    ["<A-'>"] = { function()
        require("nvterm.terminal").toggle "horizontal"
    end,
      "Open terminal"
    },

    ["<leader>rr"] = {
      function()
        vim.lsp.buf.rename()
      end,
      "LSP refactor -> rename",
      opts = { nowait = true, silent = true}
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
    ["<D-w>"] = { ":call Close_win_or_buf()<CR>", "Close window", opts = {silent = true}},
    ["<C-x>@sw"] = { ":call Close_win_or_buf()<CR>", "Close window", opts = {silent = true}},
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
    ["<C-x>@sg"] = {
      "<cmd> Neogit <CR>",
      "Open Neogit",
      opts = {silent = true}
    }
  },

  i = {
    ["<D-q>"] = { "<ESC>", "Enter normal mode"},
    ["<C-k>"] = { "<End>", "Move to end of line" },
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
    ["<D-w>"] = { "<C-o>:call Close_win_or_buf()<CR>", "Close window", opts = {silent = true}},
    ["<C-x>@sw"] = { "<C-o>:call Close_win_or_buf()<CR>", "Close window", opts = {silent = true}},
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
    ["<D-q>"] = { "<ESC>", "Enter normal mode"},
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

    ["<A-'>"] = { function()
        require("nvterm.terminal").toggle "horizontal"
     end,
     "Open terminal"
    },

    ["<A-h>"] = { "<C-\\><C-n><C-w>h", "Move to left window"},
    ["<A-j>"] = { "<C-\\><C-n><C-w>j", "Move to down window"},
    ["<A-k>"] = { "<C-\\><C-n><C-w>k", "Move to up window"},
    ["<A-l>"] = { "<C-\\><C-n><C-w>l", "Move to right window"},
  }
}

M.general[{"n", "v", "i"}] = {
    ["<D-j>"] = { "10j", "Move down 10 lines" },
    ["<D-k>"] = { "10k", "Move up 10 lines" },
    ["<F1>"] = { '<cmd> exec "NvimTreeToggle " . getcwd()<CR>', "Toggle file explorer", opts = { silent = true, nowait = true} },
    ["<C-c>"] = { "<ESC>", "ESC"},
    ["<C-x><C-x>"] = { "<ESC>:qa!<CR>", "Force quit all"},
    ["<C-x><C-s>"] = { "<ESC>:w<CR>", "Save"},
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
    ["<leader>ff"] = { function()
        require'telescope.builtin'.find_files({find_command = {'rg', '--files', '--hidden', '-g', '!.git', '-g', '!.github', '-g', '!node_modules' }, cwd = vim.fn.expand('%:p:h')})
    end, "Find files", opts = { nowait = true, silent = true} },
    ["<D-f>"] = { function()
        require'telescope.builtin'.find_files({find_command = {'rg', '--files', '--hidden', '-g', '!.git', '-g', '!.github', '-g', '!node_modules' }, cwd = vim.fn.expand('%:p:h')})
    end, "Find files", opts = { nowait = true, silent = true} },
    ["<C-x>@sf"] = { function()
        require'telescope.builtin'.find_files({find_command = {'rg', '--files', '--hidden', '-g', '!.git', '-g', '!.github', '-g', '!node_modules' }, cwd = vim.fn.expand('%:p:h')})
    end, "Find files", opts = { nowait = true, silent = true} },
    ["<C-x><C-f>"] = { function()
        require'telescope.builtin'.find_files({find_command = {'rg', '--files', '--hidden', '-g', '!.git', '-g', '!.github', '-g', '!node_modules' }, cwd = vim.fn.expand('%:p:h')})
    end, "Find files", opts = { nowait = true, silent = true} },

    ["<leader>fa"] = { function()
        require'telescope.builtin'.find_files({find_command = {'rg', '--files', '--hidden', '-g', '!.git', '-g', '!.github', '-g', '!node_modules' }})
    end, "Find all", opts = { nowait = true, silent = true} },
    ["<leader>pf"] = { function()
        require'telescope.builtin'.find_files({find_command = {'rg', '--files', '--hidden', '-g', '!.git', '-g', '!.github', '-g', '!node_modules' }})
    end, "Find all", opts = { nowait = true, silent = true} },
    ["<D-o>"] = { function()
        require'telescope.builtin'.find_files({find_command = {'rg', '--files', '--hidden', '-g', '!.git', '-g', '!.github', '-g', '!node_modules' }})
    end, "Find all", opts = { nowait = true, silent = true} },
    ["<C-x>@so"] = { function()
        require'telescope.builtin'.find_files({find_command = {'rg', '--files', '--hidden', '-g', '!.git', '-g', '!.github', '-g', '!node_modules' }})
    end, "Find all", opts = { nowait = true, silent = true} },

    ["<leader>/"] = { "<cmd> Telescope live_grep <CR>", "Live grep" , opts = { nowait = true, silent = true} },
    ["<leader>*"] = { "<cmd> Telescope grep_string <CR>", "Grep string" , opts = { nowait = true, silent = true} },
    ["<leader>bb"] = { "<cmd> Telescope buffers <CR>", "Find buffers" , opts = { nowait = true, silent = true} },
    ["<D-b>"] = { "<cmd> Telescope buffers <CR>", "Find buffers" , opts = { nowait = true, silent = true} },
    ["<C-x>@sb"] = { "<cmd> Telescope buffers <CR>", "Find buffers" , opts = { nowait = true, silent = true} },
    ["<C-x><C-b>"] = { "<cmd> Telescope buffers <CR>", "Find buffers" , opts = { nowait = true, silent = true} },
    ["<leader>fh"] = { "<cmd> Telescope help_tags <CR>", "Help page" , opts = { nowait = true, silent = true} },
    ["<leader>fr"] = { "<cmd> Telescope oldfiles <CR>", "Find oldfiles" , opts = { nowait = true, silent = true} },
    ["<leader>ss"] = { "<cmd> Telescope current_buffer_fuzzy_find <CR>", "Find in current buffer" , opts = { nowait = true, silent = true} },

    -- copy path of current file to system clipboard
    ["<leader>fyy"] = { function()
        local path = vim.fn.expand "%:p"
        vim.fn.setreg("+", path)
        vim.notify("Copied path to clipboard: " .. path)
    end,
      "Copy path to clipboard"
    },

    -- copy base name of current file to system clipboard
    ["<leader>fyn"] = { function()
        local path = vim.fn.expand "%:t"
        vim.fn.setreg("+", path)
        vim.notify("Copied base name to clipboard: " .. path)
    end,
      "Copy base name to clipboard"
    },

    -- git
    ["<leader>gl"] = { "<cmd> Telescope git_commits <CR>", "Git commits" , opts = { nowait = true, silent = true} },
    ["<leader>gs"] = { "<cmd> Neogit <CR>", "Git status" , opts = { nowait = true, silent = true} },
    ["<D-g>"] = { "<cmd> Neogit <CR>", "Git status" , opts = { nowait = true, silent = true} },

    -- pick a hidden term
    ["<leader>pt"] = { "<cmd> Telescope terms <CR>", "Pick hidden term" , opts = { nowait = true, silent = true} },

    -- theme switcher
    ["<leader>th"] = { "<cmd> Telescope themes <CR>", "Nvchad themes" , opts = { nowait = true, silent = true} },

    ["<leader>fb"] = { "<cmd> Telescope marks <CR>", "telescope bookmarks" , opts = { nowait = true, silent = true} },
    ["<D-i><D-b>"] = { "<cmd> Telescope marks <CR>", "telescope bookmarks" , opts = { nowait = true, silent = true} },
    ["<leader>w|"] = { "<C-o>:belowright vsplit | wincmd w<CR>", "Split window vertically", opts = { nowait = true, silent = true} },
    ["<leader>w-"] = { "<C-o>:belowright split | wincmd w<CR>", "Split window horizontally", opts = { nowait = true, silent = true} },
    ["<leader>ji"] = { "<cmd> Telescope ctags_outline outline<CR>", "Jump to ctags outline", opts = { nowait = true, silent = true} },
  },

  v = {
    ["<leader>*"] = { "<cmd> Telescope grep_string <CR>", "Grep string" , opts = { nowait = true, silent = true} },
  }
}

M.hop = {
  n = {
    ["<leader><leader>"] = { "<cmd> HopChar2<CR>", "Jump to char", opts = { silent = true, nowait = true}},
    ["<leader>jl"] = { "<cmd> HopLine<CR>", "Jump to char", opts = { silent = true, nowait = true}},
    ["<D-l>"] = { "<cmd> HopLine<CR>", "Jump to char", opts = { silent = true, nowait = true}},
    ["<C-x>@sl"] = { "<cmd> HopLine<CR>", "Jump to char", opts = { silent = true, nowait = true}},
  }
}

M.tabufline = {
  n = { ["<leader>x"] = {""}}
}

M.tabularize = {}
M.tabularize[{"n", "v"}] = {
    ["<leader>xa#"] = {"<cmd> Tabularize /#<CR>", "Align by #", opts = { silent = true, nowait = true} },
    ["<leader>xa:"] = {"<cmd> Tabularize /:<CR>", "Align by :", opts = { silent = true, nowait = true} },
    ["<leader>xa="] = {"<cmd> Tabularize /=<CR>", "Align by =", opts = { silent = true, nowait = true} },
    ["<leader>xa,"] = {"<cmd> Tabularize /,<CR>", "Align by ,", opts = { silent = true, nowait = true} },
    ["<leader>xa|"] = {"<cmd> Tabularize /|<CR>", "Align by |", opts = { silent = true, nowait = true} },
    ["<leader>xa:"] = {"<cmd> Tabularize /:<CR>", "Align by :", opts = { silent = true, nowait = true} },
    ["<leader>xa."] = {"<cmd> Tabularize /.<CR>", "Align by .", opts = { silent = true, nowait = true} },
    ["<leader>xa;"] = {"<cmd> Tabularize /;<CR>", "Align by ;", opts = { silent = true, nowait = true} },
    ["<leader>xa-"] = {"<cmd> Tabularize /-<CR>", "Align by -", opts = { silent = true, nowait = true} },
    ["<leader>xa_"] = {"<cmd> Tabularize /_<CR>", "Align by _", opts = { silent = true, nowait = true} },
    ["<leader>xa+"] = {"<cmd> Tabularize /+<CR>", "Align by +", opts = { silent = true, nowait = true} },
    ["<leader>xa*"] = {"<cmd> Tabularize /*<CR>", "Align by *", opts = { silent = true, nowait = true} },
    ["<leader>xa/"] = {"<cmd> Tabularize //\\zs<CR>", "Align by /", opts = { silent = true, nowait = true} },
    ["<leader>xa "] = {"<cmd> Tabularize /\\s\\ze\\S/l0<CR>", "Align by space", opts = { silent = true, nowait = true} },
    ["<leader>xa\\"] = {"<cmd> Tabularize /\\\\<CR>", "Align by \\", opts = { silent = true, nowait = true} },
}


return vim.tbl_deep_extend("force", M, SpaceKeyMappings)
