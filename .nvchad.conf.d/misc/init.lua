--define a vimscript function
vim.cmd([[
  fu! SelectTab(num) abort
      let ls = split(execute(':ls'), "\n")
      let buffers = []
      for b in ls
        let nr = matchstr(b, '\d\+')
        call add(buffers, nr)
      endfor
      if a:num == 0
        exec 'buffer ' . buffers[-1]
      else
        if len(buffers) >= a:num
          exec 'buffer ' . buffers[a:num - 1]
        endif
      endif
  endf
]])

vim.api.nvim_set_keymap("", "<D-1>", ":call SelectTab(1)<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("", "<D-2>", ":call SelectTab(2)<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("", "<D-3>", ":call SelectTab(3)<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("", "<D-4>", ":call SelectTab(4)<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("", "<D-5>", ":call SelectTab(5)<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("", "<D-6>", ":call SelectTab(6)<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("", "<D-7>", ":call SelectTab(7)<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("", "<D-8>", ":call SelectTab(8)<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("", "<D-9>", ":call SelectTab(9)<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("", "<D-0>", ":call SelectTab(0)<CR>", { noremap = true, silent = true })
