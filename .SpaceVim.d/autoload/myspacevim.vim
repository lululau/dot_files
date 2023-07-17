function! myspacevim#before() abort
  autocmd User NerdTreeInit
     \ nnoremap <silent><buffer> <CR> :<C-u>call
     \ g:NERDTreeKeyMap.Invoke('o')<CR>
endfunction

function! myspacevim#after() abort
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

  inoremap <c-e> <c-o>A
  inoremap <c-a> <c-o>^
  inoremap <c-f> <Right>
  inoremap <c-b> <Left>
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
  nmap <c-x>@c gdk
  imap <c-x>@c <esc>gdk
  nmap <return> ]<Space>

endfunction
