for i = 1, 9, 1 do
  vim.api.nvim_set_keymap("", string.format("<A-%s>", i), ":lua vim.api.nvim_set_current_buf(vim.t.bufs["..i.."])<CR>", { noremap = true, silent = true })
  vim.api.nvim_set_keymap("", string.format("<D-%s>", i), ":lua vim.api.nvim_set_current_buf(vim.t.bufs["..i.."])<CR>", { noremap = true, silent = true })
end

vim.api.nvim_set_keymap("", "<M-Return>", ":Copilot panel<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("i", "<M-Return>", "<ESC>:Copilot panel<CR>", { noremap = true, silent = true })

vim.api.nvim_set_keymap("n", "<leader>pp", "<cmd> Telescope projects<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<D-p>", "<cmd> Telescope projects<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("i", "<D-p>", "<cmd> Telescope projects<CR>", { noremap = true, silent = true })

vim.api.nvim_set_keymap("", "<C-g>", "<C-c>:nohlsearch<CR>", { noremap = true, silent = true })


vim.cmd("highlight CopilotSuggestion guifg=#24ead9 ctermfg=6")
vim.cmd("au TextYankPost * silent! lua vim.highlight.on_yank()")

vim.g.vim_textobj_parameter_mapping = 'a'


if vim.g.neovide then
  for i = 1, 9 do
    vim.api.nvim_set_keymap('n', '<A-'..i..'>', ':'..i .. 'wincmd w<cr>', {noremap = true, silent = true})
    vim.api.nvim_set_keymap('i', '<C-o><A-'..i..'>', ':'..i .. 'wincmd w', {noremap = true, silent = true})
  end
end

-- bind <C-x><C-k> to require("nvchad.tabufline").closeOtherBufs()
vim.api.nvim_set_keymap("n", "<C-x><C-k>", ":lua require('nvchad.tabufline').closeOtherBufs()<CR>", { noremap = true, silent = true })

