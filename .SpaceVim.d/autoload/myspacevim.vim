function! myspacevim#before() abort

  set ignorecase smartcase

  nnoremap <A-x> :tabonly<CR>

  if exists("g:neovide")
    let g:spacevim_guifont = "JetBrainsMono\\ Nerd\\ Font:h13"
    let g:neovide_transparency = 0.0
    let g:transparency = 1.0
    let g:neovide_background_color = '#272c36'.printf('%x', float2nr(255 * g:transparency))
    let g:neovide_input_macos_alt_is_meta = v:true

    lua <<EOF
      vim.keymap.set('n', '<D-s>', ':w<CR>') -- Save
      vim.keymap.set('v', '<D-c>', '"+y') -- Copy
      vim.keymap.set('n', '<D-v>', '"+P') -- Paste normal mode
      vim.keymap.set('v', '<D-v>', '"+P') -- Paste visual mode
      vim.keymap.set('c', '<D-v>', '<C-R>+') -- Paste command mode
      vim.keymap.set('i', '<D-v>', '<ESC>l"+Pli') -- Paste insert mode

      vim.keymap.set('i', '<D-j>', '<C-o>10j')
      vim.keymap.set('i', '<D-k>', '<C-o>10k')
      vim.keymap.set('n', '<D-j>', '10j')
      vim.keymap.set('n', '<D-k>', '10k')
      vim.keymap.set('v', '<D-j>', '10j')
      vim.keymap.set('v', '<D-k>', '10k')

      vim.keymap.set('n', '<D-x>', ':tabonly<CR>')

      vim.keymap.set('n', '<C-CR>', ':call SpaceVim#mapping#gd()<CR>')
      vim.keymap.set('n', '<D-CR>', ':call SpaceVim#lsp#go_to_typedef()<CR>')
      vim.keymap.set('n', '<S-CR>', ':call SpaceVim#lsp#references()<CR>')
      vim.keymap.set('i', '<C-CR>', ':call SpaceVim#mapping#gd()<CR>')
      vim.keymap.set('i', '<D-CR>', ':call SpaceVim#lsp#go_to_typedef()<CR>')
      vim.keymap.set('i', '<S-CR>', ':call SpaceVim#lsp#references()<CR>')

      vim.keymap.set('n', '<D-t>', ':belowright vsplit | wincmd w<CR>')
      vim.keymap.set('n', '<D-w>', ':close<CR>')
      vim.keymap.set('i', '<D-t>', '<c-o>:belowright vsplit | wincmd w<CR>')
      vim.keymap.set('i', '<D-w>', '<c-o>:close<CR>')

      vim.keymap.set('n', '<D-d>', ":call SpaceVim#mapping#close_current_buffer()<cr>")
      vim.keymap.set('n', '<D-b>', ":Telescope buffers<cr>")
      vim.keymap.set('n', '<D-f>', ":exe 'Telescope find_files cwd=' . fnamemodify(bufname('%'), ':p:h')<cr>")
      vim.keymap.set('n', '<D-g>', ":Gina status --opener=vsplit<cr>")
      vim.keymap.set('n', '<D-l>', ":HopLine<cr>")
      vim.keymap.set('n', '<D-p>', ":call SpaceVim#plugins#projectmanager#list()<cr>")
      vim.keymap.set('n', '<D-o>', ":Telescope find_files<cr>")
EOF
endif

  let g:multi_cursor_use_default_mapping=0
  let g:multi_cursor_start_word_key      = '<A-J>'
  let g:multi_cursor_select_all_word_key = '<A-m>'
  let g:multi_cursor_start_key           = 'g<A-J>'
  let g:multi_cursor_select_all_key      = 'g<A-m>'
  let g:multi_cursor_next_key            = '<A-J>'
  let g:multi_cursor_prev_key            = '<A-K>'
  let g:multi_cursor_skip_key            = '<A-X>'
  let g:multi_cursor_quit_key            = '<Esc>'

  autocmd User NerdTreeInit
  \ nnoremap <silent><buffer> <CR> :<C-u>call
  \ g:NERDTreeKeyMap.Invoke('o')<CR>
endfunction


function! myspacevim#after() abort

  " if firenvim (it sets the g:started_by_firenvim variable)
  if exists('g:started_by_firenvim')
    imap « <Plug>(copilot-suggest)
    imap ‘ <Plug>(copilot-next)
    imap “ <Plug>(copilot-previous)
    lua <<EOF
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
      vim.cmd "set guifont=JetBrainsMono\\ Nerd\\ Font:h12"
    end
EOF
  endif
  map <F1> :NERDTreeToggle<CR>
  " nnoremap <F3> :set invpaste paste?<CR>
  " set pastetoggle=<F3>
  map <c-c> <esc>
  nmap <c-x><c-c> <ESC>:qa<CR>
  imap <c-x><c-c> <ESC>:qa<CR>
  noremap <c-x><c-x> <ESC>:qa!<CR>
  inoremap <c-x><c-x> <ESC>:qa!<CR>
  noremap <c-x><c-s> <ESC>:w<CR>
  inoremap <c-x><c-s> <c-o>:w<CR>
  snoremap <c-x><c-s> <c-o>:w<CR>
  noremap <c-x><c-k> :bw!<CR>
  inoremap <c-x><c-k> <ESC>:bw!<CR>
  imap <c-n> <c-j>
  imap <c-p> <c-k>
  cmap <c-n> <c-j>
  cmap <c-p> <c-k>
  nmap <c-x><c-o><c-e> :call SpaceVim#mapping#gd()<CR>
  imap <c-x><c-o><c-e> <c-o>:call SpaceVim#mapping#gd()<CR>
  nmap <c-x><c-o><c-a> :call SpaceVim#lsp#go_to_typedef()<CR>
  " nmap <c-x><c-o><c-a> :call SpaceVim#mapping#g_capital_d()<CR>
  imap <c-x><c-o><c-a> <c-o>:call SpaceVim#lsp#go_to_typedef()<CR>
  " imap <c-x><c-o><c-a> <c-o>:call SpaceVim#mapping#g_capital_d()<CR>
  nmap <c-x><c-o><c-b> :call SpaceVim#lsp#references()<CR>
  imap <c-x><c-o><c-b> <c-o>:call SpaceVim#lsp#references()<CR>
  nnoremap <c-x><c-z> <c-z>
  imap <c-z> <esc>
  nnoremap <c-z> i
  nmap <A-h> :wincmd h<CR>
  nmap <A-j> :wincmd j<CR>
  nmap <A-k> :wincmd k<CR>
  nmap <A-l> :wincmd l<CR>
  " nmap <c-z> i
  " nnoremap <c-z><c-z> <c-z>

  inoremap <c-e> <c-o>A
  inoremap <c-a> <c-o>^
  nnoremap <c-a> ^
  inoremap <c-f> <Right>
  inoremap <c-b> <Left>
  cnoremap <c-p> <up>
  cnoremap <c-n> <down>
  inoremap <c-d> <Del>
  cnoremap <c-b> <Left>
  cnoremap <c-f> <Right>
  cnoremap <c-a> <c-b>


  noremap <A-tab> :b#<CR>
  inoremap <A-tab> <ESC>:b#<CR>


  inoremap <A-f> <c-o>e
  cnoremap <A-f> <S-Right>
  inoremap <A-b> <c-o>b
  cnoremap <A-b> <S-Left>
  inoremap <A-<> <c-o>gg<c-o>0
  inoremap <A->> <c-o>G<c-o>$

  noremap <A-w> <c-w>
  inoremap <A-w> <c-w>
  inoremap <A-bs> <esc><esc>caw
  cnoremap <A-bs> <c-w>

  call SpaceVim#mapping#space#def('xnoremap', ['y'], 'call clipboard#yank()', 'copy-to-system-clipboard', 1)

  call SpaceVim#mapping#space#def('nnoremap', ['/'], 'call SpaceVim#plugins#flygrep#open({})',
  \ 'grep-on-the-fly', 1)

  call SpaceVim#mapping#space#def('nnoremap', ['*'],
       \ "call SpaceVim#plugins#flygrep#open({'input' : expand(\"<cword>\"), 'dir' : get(b:, \"rootDir\", getcwd())})",
       \ 'grep-cword-in-project', 1)



  call SpaceVim#mapping#space#def('nnoremap', ['n', 'i'], ':NERDTreeFind<cr>', 'find-file-in-nerdtree', 1)

  call SpaceVim#mapping#space#def('nmap', ['<Space>'], 'HopChar2', 'jump-to-suite-of-two-characters', 1, 1)

  highlight CopilotSuggestion guifg=#24ead9 ctermfg=6
  nnoremap <M-return> :Copilot panel<CR>
  inoremap <M-return> <esc>:Copilot panel<CR>
  iabbrev pry require 'pry'; binding.pry;<ESC>
  iabbrev ipdb import ipdb; ipdb.set_trace()<ESC>
  " nmap <return> ]<Space>

  if exists("g:neovide")
    nnoremap <A-1> :call SpaceVim#layers#core#statusline#jump("1")<CR>
    nnoremap <A-2> :call SpaceVim#layers#core#statusline#jump("2")<CR>
    nnoremap <A-3> :call SpaceVim#layers#core#statusline#jump("3")<CR>
    nnoremap <A-4> :call SpaceVim#layers#core#statusline#jump("4")<CR>
    nnoremap <A-5> :call SpaceVim#layers#core#statusline#jump("5")<CR>
    nnoremap <A-6> :call SpaceVim#layers#core#statusline#jump("6")<CR>
    nnoremap <A-7> :call SpaceVim#layers#core#statusline#jump("7")<CR>
    nnoremap <A-8> :call SpaceVim#layers#core#statusline#jump("8")<CR>
    nnoremap <A-9> :call SpaceVim#layers#core#statusline#jump("9")<CR>
  endif


endfunction
