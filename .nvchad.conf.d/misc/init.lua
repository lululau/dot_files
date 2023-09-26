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

vim.cmd([[
  if has('patch-7.4-2215') " && exists('*getwininfo')
    function! Get_qf_winnr() abort
      let wins = filter(getwininfo(), 'v:val.quickfix && !v:val.loclist')
      " assert(len(wins) <= 1)
      return empty(wins) ? 0 : wins[0].winnr
    endfunction
  else
    function! Get_qf_winnr() abort
      let buffers = split(self.__cmp.execute('ls!'), "\n")
      call filter(buffers, 'v:val =~# "\\V[Quickfix List]"')
      " :cclose removes the buffer from the list (in my config only??)
      " assert(len(buffers) <= 1)
      return empty(buffers) ? 0 : eval(matchstr(buffers[0], '\v^\s*\zs\d+'))
    endfunction
  endif

  function! Close_quickfix() abort
    if winnr() == Get_qf_winnr()
      cclose
    else
      lclose
    endif
  endfunction

  augroup SpaceVim_core
    au!
    autocmd BufWinEnter quickfix nnoremap <silent> <buffer>
          \   q :call Close_quickfix()<cr>
    autocmd BufEnter * if (winnr('$') == 1 && &buftype ==# 'quickfix' ) |
          \   bd|
          \   q | endif
  augroup END
]])
