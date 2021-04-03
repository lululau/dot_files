"
" Author:         Larry Lv <larrylv1990@gmail.com>
" Last Modified:  May 14, 2014
"

filetype off " required by vundle

let s:uname = substitute(system('uname'), "\n", "", "")

if s:uname == 'Linux'
   set nocp
endif

if s:uname == 'Linux' && !empty(glob('~/liuxiang/'))
  set runtimepath+=~/liuxiang/.vim,~/liuxiang/.vim/bundle/vundle/
  let $GEM_HOME = '/home/deploy/.rvm/gems/ruby-2.2.2'
  let $GEM_PATH = '/home/deploy/.rvm/gems/ruby-2.2.2:/home/deploy/.rvm/gems/ruby-2.2.2@global'
  call vundle#rc('~/liuxiang/.vim/bundle')
else
  let $GEM_HOME = '/Users/liuxiang/.rvm/gems/ruby-2.3.2'
  let $GEM_PATH = '/Users/liuxiang/.rvm/gems/ruby-2.3.2:/Users/liuxiang/.rvm/gems/ruby-2.3.2@global'
  set runtimepath+=~/.vim/bundle/vundle/
  call vundle#rc()
endif

" vim-scripts repos
Bundle 'bufexplorer.zip'
Bundle 'matchit.zip'
Bundle 'toyamarinyon/vim-swift'

" original repos on GitHub
Bundle 'larrylv/ShowMarks'
" Bundle 'wincent/Command-T'
Bundle 'kien/ctrlp.vim'
Bundle 'tpope/vim-ragtag'
Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-markdown'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-haml'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-speeddating'
Bundle 'terryma/vim-multiple-cursors'
" Bundle 'tpope/vim-abolish'
" Bundle 'tpope/vim-dispatch'
Bundle 'tpope/vim-obsession'
Bundle 'tpope/vim-unimpaired'
Bundle 'Shougo/neocomplcache'
Bundle 'Shougo/neosnippet'
Bundle 'Shougo/neosnippet-snippets'
Bundle 'scrooloose/nerdtree'
Bundle 'scrooloose/syntastic'
Bundle 'tomtom/tcomment_vim'
Bundle 'honza/vim-snippets'
Bundle 'rking/ag.vim'
Bundle 'Lokaltog/vim-easymotion'
Bundle 'bling/vim-airline'
" Bundle 'flazz/vim-colorschemes'
Bundle 'vim-ruby/vim-ruby'
Bundle 'pangloss/vim-javascript'
Bundle 'kchmck/vim-coffee-script'
Bundle 'docunext/closetag.vim'
Bundle 'godlygeek/tabular'
Bundle 'Townk/vim-autoclose'
Bundle 'tudorprodan/html_annoyance.vim'
Bundle 'majutsushi/tagbar'
Bundle 'hail2u/vim-css3-syntax'
Bundle 'airblade/vim-gitgutter'
Bundle 'ervandew/supertab'
Bundle 'michaeljsmith/vim-indent-object'
" Bundle 'altercation/vim-colors-solarized'
Bundle 'ecomba/vim-ruby-refactoring'
Bundle 'mattn/webapi-vim'
Bundle 'mattn/gist-vim'
Bundle 'benmills/vimux'
Bundle 'groenewege/vim-less'
Bundle 'jnwhiteh/vim-golang'
Bundle 'wting/rust.vim'
Bundle 'toggle_maximize.vim'
Bundle 'rizzatti/dash.vim'
" Plugin 'szw/vim-ctrlspace'
Plugin 'tmhedberg/indent-motion'
Plugin 'nathanaelkane/vim-indent-guides'
Plugin 'guns/vim-clojure-static'
Plugin 'tpope/vim-bundler'
Plugin 'msanders/cocoa.vim'
Plugin 'DrawIt'
Plugin 'rcyrus/snipmate-snippets-rubymotion'
Plugin 'lululau/my-text-objects'
Plugin 'jgdavey/vim-blockle'
" Plugin 'blueyed/vim-diminactive'
Plugin 'tpope/vim-endwise'
Plugin 'vim-perl/vim-perl'
Plugin 'justinmk/vim-ipmotion'
Plugin 'chrisbra/csv.vim'
Plugin 'jreybert/vimagit'

set hidden

let g:airline_exclude_preview = 1
let g:multi_cursor_quit_key='<c-c>'

let g:blockle_mapping = '<Leader>bb'

set t_Co=256
set background=dark

" set wombat256 colorscheme
" colorscheme wombat256mod

let g:ragtag_global_maps = 1

" set solarized theme
" let g:solarized_termtrans = 1
" let g:solarized_termcolors = 256
" let g:solarized_visibility = "high"
" let g:solarized_contrast = "high"
" colorscheme solarized

" basic configuration
syntax on
set nocompatible
set nu
set ruler
set nobackup
set bs=2
set backspace=indent,eol,start   " Allow backspacing over everything in insert mode
" set diffopt+=iwhite              " Ignore whitespaces with vimdiff
set nojoinspaces                 " Prevents inserting two spaces after punctuation on a join (J)
set autoread                     " Reload files changed outside automatically
set scrolloff=3                  " Always shows 5 lines above/below the cursor
set showcmd                      " display incomplete commands
set tags=./tags;
set timeout timeoutlen=1000 ttimeoutlen=50 " Fix slow 0 inserts
set complete=.,w,b,u,t,i
set completeopt=longest,menu
set laststatus=2
set modeline

set fillchars-=vert:\|

set cursorline                   " Highlight current line
highlight clear SignColumn       " SignColumn should match background for things
                                 " like vim-gitgutter
highlight clear LineNr
highlight LineNr ctermfg=10

" tab/indent configuration
set expandtab
set tabstop=2
set shiftwidth=2
set softtabstop=2
set autoindent
set cindent
set encoding=utf-8
set fileencoding=utf-8
set fileencodings=utf-8,ucs-bom,chinese
set formatoptions+=mM
set ambiwidth=double
set noshowmode

filetype plugin indent on

" search configuration
set smartcase
set hlsearch
set incsearch
set ignorecase

let g:fugitive_github_domains = ['http://github.ktjr.com']


" vim-powerline configurations
" let g:Powerline_symbols = 'fancy'
" let g:Powerline_colorscheme = 'solarized256'
" call Pl#Theme#RemoveSegment('fugitive:branch')
" call Pl#Theme#RemoveSegment('fileformat')
" let g:Powerline_stl_path_style = 'filename'

" vim-airline configurations
" let g:airline_theme='powerlineish'
let g:airline_theme='powerlineish'
let g:airline_powerline_fonts = 1
let g:airline#extensions#hunks#enabled = 0
let g:airline#extensions#whitespace#enabled = 0
let g:airline_powerline_fonts = 1
let g:airline_left_sep = '⮀'
let g:airline_left_alt_sep = '⮁'
let g:airline_right_sep = '⮂'
let g:airline_right_alt_sep = '⮃'
let g:airline_symbols = {}
let g:airline_symbols.space = ' '
let g:airline_symbols.branch = '⭠'
let g:airline_symbols.readonly = '⭤'
let g:airline_symbols.linenr = '⭡'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = ' '
let g:airline#extensions#tabline#formatter = 'unique_tail'

" let g:airline_left_sep = '▶'
" let g:airline_left_alt_sep = '❯'
" let g:airline_right_sep = '◀'
" let g:airline_right_alt_sep = '❮'
" let g:airline_symbols = {}
" let g:airline_symbols.space = ' '
" let g:airline_symbols.branch = '⚡'
" let g:airline_symbols.readonly = '☓'
" let g:airline_symbols.linenr = '⁋'

" Tab triggers buffer-name auto-completion
set wildchar=<Tab> wildmenu wildmode=full

" Undo file settings
set undodir=~/.vim/.undo
set undofile
set undolevels=1000
set undoreload=10000


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" MISC KEY MAPS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

function! GotoBuffer(buf)
  let s:visible_buffers = airline#extensions#tabline#buflist#list()
  let s:current_buffer = bufnr("%")
  let s:current_buffer_index = index(s:visible_buffers, s:current_buffer)
  if a:buf == '>'

    if s:current_buffer_index == len(s:visible_buffers) - 1
      let s:buf_num = s:visible_buffers[0]
    else
      let s:buf_num = s:visible_buffers[s:current_buffer_index+1]
    endif
  elseif a:buf == '<'
    let s:buf_num = s:visible_buffers[s:current_buffer_index-1]
  elseif a:buf == 'L'
    let s:buf_num = s:visible_buffers[-1]
  else
    let s:buf_num = s:visible_buffers[a:buf-1]
  endif
  execute 'normal :b' . s:buf_num . "\r"
endfunction

inoremap <c-c> <ESC>
" imap jj <ESC>
" Insert a hash rocket with <c-h>
inoremap <c-h> =><space>
" Move around splits with <c-hjkl>
noremap <C-k> <C-w><Up>
noremap <C-j> <C-w><Down>
noremap <C-l> <C-w><Right>
noremap <C-h> <C-w><Left>
" Clear the search buffer when hitting return
function! MapCR()
  nnoremap <c-g> :nohlsearch<cr>
endfunction
call MapCR()

nmap <F2> :TagbarToggle<CR>
nnoremap <F3> :set invpaste paste?<CR>
set pastetoggle=<F3>
nnoremap <silent> <F4> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>
map <F5> :!ctags -e -R --languages=-javascript --langmap=ruby:+.rake --exclude=.git --exclude=log --exclude=target --fields=+iaS --extra=+q .<CR>
nnoremap <Left> :tprevious<CR>
nnoremap <Right> :tnext<CR>

" increase number, <c-a> is prefix for tmux.
" map <c-i> <c-a>

" map <C-x> <C-s> <esc>:w<CR>
" imap <C-x> <C-s> <esc>:w<CR>


" force write and save
cnoremap w!! %!sudo tee > /dev/null %

let mapleader=" "
noremap <leader>y "*y
noremap <leader>p :echo @%<cr>

" Open .vimrc for quick-edit.
noremap <leader>se :edit $MYVIMRC<cr>
noremap <leader>so :source $MYVIMRC<cr>
noremap <leader>ss :source ./Session.vim<cr>

" Some helpers to edit mode
cnoremap %% <C-R>=expand('%:h').'/'<cr>
noremap <leader>ew :e %%
noremap <leader>es :sp %%
noremap <leader>ev :vsp %%
noremap <leader>et :tabe %%

" remember last location when open a file
autocmd BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$")
      \| exe "normal g'\"" | endif

command! GdiffInTab tabedit %|vsplit|Gdiff
nnoremap <leader>d :GdiffInTab<cr>
nnoremap <leader>D :tabclose<cr>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" RENAME CURRENT FILE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! RenameFile()
    let old_name = expand('%')
    let new_name = input('New file name: ', expand('%'), 'file')
    if new_name != '' && new_name != old_name
        exec ':saveas ' . new_name
        exec ':silent !rm ' . old_name
        redraw!
    endif
endfunction
" noremap <leader>n :call RenameFile()<cr>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" CLOSE QUICKFIX WINDOW
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
noremap <leader>cc :ccl<cr>
au FileType qf call AdjustWindowHeight(3, 10)
function! AdjustWindowHeight(minheight, maxheight)
  exe max([min([line("$"), a:maxheight]), a:minheight]) . "wincmd _"
endfunction

" Insert the current time
command! InsertTime :normal a<c-r>=strftime('%F %H:%M:%S')<cr>

let g:SuperTabDefaultCompletionType = "<c-n>"


" vim-gitgutter configurations
noremap <leader>ggn :GitGutterNextHunk<cr>
noremap <leader>ggp :GitGutterPrevHunk<cr>


" bufExplorer configurations
let g:bufExplorerShowTabBuffer=1    " BufExplorer: show only buffers relative to this tab
let g:bufExplorerShowRelativePath=1 " BufExplorer: show relative paths


" rails.vim configurations
noremap <leader>c :Econtroller<cr>
noremap <leader>v :Eview<cr>
noremap <leader>m :Emodel<cr>
" map <leader>h :Rhelper<cr>

" gist-vim configurations
let g:gist_post_private = 1
let g:gist_show_privates = 1
let g:gist_open_browser_after_post = 1
let g:gist_detect_filetype = 1

" vim-easymotion configurations
" let g:EasyMotion_leader_key = '\\'
" let g:EasyMotion_mapping_f = 't'
" let g:EasyMotion_mapping_F = 'T'
" let g:EasyMotion_mapping_j = '<leader>j'
" let g:EasyMotion_mapping_k = '<leader>k'
" let g:EasyMotion_mapping_n = '<leader>n'
" let g:EasyMotion_mapping_N = '<leader>N'
let g:EasyMotion_smartcase = 1
map <Leader>e <Plug>(easymotion-prefix)
map <Leader>l <Plug>(easymotion-bd-jk)
nmap <Leader><Space> <Plug>(easymotion-s2)


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" SWITCH BETWEEN TEST AND PRODUCTION CODE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! OpenTestAlternate()
  let new_file = AlternateForCurrentFile()
  exec ':e ' . new_file
endfunction
function! AlternateForCurrentFile()
  let current_file = expand("%")
  let new_file = current_file
  let in_spec = match(current_file, '" spec/') != -1
  let going_to_spec = !in_spec
  let in_app = match(current_file, '\<controllers\>') != -1 || match(current_file, '\<models\>') != -1 || match(current_file, '\<views\>') || match(current_file, '\<helpers\>') != -1
  if going_to_spec
    if in_app
      let new_file = substitute(new_file, '" app/', '', '')
    end
    let new_file = substitute(new_file, '\.rb$', '_spec.rb', '')
    let new_file = 'spec/' . new_file
  else
    let new_file = substitute(new_file, '_spec\.rb$', '.rb', '')
    let new_file = substitute(new_file, '" spec/', '', '')
    if in_app
      let new_file = 'app/' . new_file
    end
  endif
  return new_file
endfunction
nnoremap <leader>. :call OpenTestAlternate()<cr>
" nnoremap <leader>s :call OpenTestAlternate()<cr>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ShowMarks.vim CONFIGURATIONS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let showmarks_enable = 1
let showmarks_include = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
" Ignore help, quickfix, non-modifiable buffers
let showmarks_ignore_type = "hqm"
" Hilight lower & upper marks
let showmarks_hlline_lower = 1
let showmarks_hlline_upper = 1
hi ShowMarksHLl ctermbg=Yellow  ctermfg=Black guibg=#FFDB72 guifg=Black
hi ShowMarksHLu ctermbg=Magenta ctermfg=Black guibg=#FFB3FF guifg=Black


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Syntastic.vim CONFIGURATIONS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set statusline+=\ %#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_check_on_open=1
let g:syntastic_enable_signs=1
let g:syntastic_auto_jump=0
let g:syntastic_mode_map = { 'mode': 'active',
                           \ 'passive_filetypes': ['tex', 'scss', 'slim'] }
let g:syntastic_error_symbol = '✗'
let g:syntastic_style_error_symbol = '✠'
let g:syntastic_warning_symbol = '∆'
let g:syntastic_style_warning_symbol = '≈'


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Tabularize.vim CONFIGURATIONS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nnoremap <Leader>t& :Tabularize /&<CR>
vnoremap <Leader>t& :Tabularize /&<CR>
nnoremap <Leader>t/ :Tabularize /\/\/<CR>
vnoremap <Leader>t/ :Tabularize /\/\/<CR>
nnoremap <Leader>t= :Tabularize /=<CR>
vnoremap <Leader>t= :Tabularize /=<CR>
nnoremap <Leader>t# :Tabularize /#<CR>
vnoremap <Leader>t# :Tabularize /#<CR>
nnoremap <Leader>t: :Tabularize /:<CR>
vnoremap <Leader>t: :Tabularize /:<CR>
nnoremap <Leader>t:: :Tabularize /:\zs<CR>
vnoremap <Leader>t:: :Tabularize /:\zs<CR>
nnoremap <Leader>t, :Tabularize /,<CR>
vnoremap <Leader>t, :Tabularize /,<CR>
nnoremap <Leader>t<Bar> :Tabularize /<Bar><CR>
vnoremap <Leader>t<Bar> :Tabularize /<Bar><CR>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" NEOCOMPLCACHE CONFIGURATIONS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
" highlight Pmenu ctermbg=238 gui=bold
highlight Pmenu guifg=#000000 guibg=#F8F8F8 ctermfg=black ctermbg=Lightgray
highlight PmenuSbar guifg=#8A95A7 guibg=#F8F8F8 gui=NONE ctermfg=darkcyan ctermbg=lightgray cterm=NONE
highlight PmenuThumb guifg=#F8F8F8 guibg=#8A95A7 gui=NONE ctermfg=lightgray ctermbg=darkcyan cterm=NONE

let g:neocomplcache_enable_at_startup = 1
let g:neocomplcache_enable_camel_case_completion = 1
let g:neocomplcache_enable_smart_case = 1
let g:neocomplcache_enable_underbar_completion = 1
let g:neocomplcache_enable_auto_delimiter = 1
let g:neocomplcache_force_overwrite_completefunc = 1

if !exists('g:neocomplcache_omni_patterns')
  let g:neocomplcache_omni_patterns = {}
endif
let g:neocomplcache_omni_patterns.ruby = '[" . *\t]\.\w*\|\h\w*::'
" Enable omni completion.
autocmd FileType ruby setlocal omnifunc=rubycomplete#Complete
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown,mkd setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
inoremap <expr><C-g>     neocomplcache#undo_completion()
inoremap <expr><C-l>     neocomplcache#complete_common_string()

" Enable snipMate compatibility feature.
let g:neosnippet#enable_snipmate_compatibility = 1
" Tell Neosnippet about the other snippets
let g:neosnippet#snippets_directory='~/.vim/bundle/vim-snippets/snippets'
" let g:neocomplcache_disable_auto_complete = 1 " Disable auto popup.


" Set snips_author & snips_email for snipMate.vim
let g:snips_author="Larry Lv"
let g:snips_email="larrylv1990@gmail.com""

" Mapping Keys
imap <silent><c-l> <Plug>(neosnippet_expand)
smap <silent><c-l> <Plug>(neosnippet_expand)
imap <silent><c-j> <Plug>(neosnippet_jump)
smap <silent><c-j> <Plug>(neosnippet_jump)

function! s:my_crinsert()
    return pumvisible() ? neocomplcache#close_popup() : "\<Cr>"
endfunction
inoremap <silent> <expr><CR> <C-R>=<SID>my_crinsert()<CR>

" inoremap <expr><CR>  neocomplcache#smart_close_popup() . "\<CR>"
inoremap <expr><C-e>  neocomplcache#close_popup()
inoremap <expr><C-y>  neocomplcache#cancel_popup()
inoremap <expr><C-c>  neocomplcache#cancel_popup()
inoremap <expr><BS> neocomplcache#smart_close_popup()."\<C-h>"


" Highlight trailing whitespace
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()

set wildignore+=*.o,*.log,*.obj,.git,*.jpg,*.png,*.gif,*/vendor/bundle,*/vendor/cache,*/public/download " exclude files from listings


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Rails Development CONFIGURATIONS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! ShowRoutes()
" Requires 'scratch' plugin
  :topleft 100 :split __Routes__
" Make sure Vim doesn't write __Routes__ as a file
  :set buftype=nofile
" Delete everything
  :normal 1GdG
" Put routes output in buffer
  :0r! zeus rake -s routes
" Size window to number of lines (1 plus rake output length)
  :exec ":normal " . line("$") . "_ "
" Move cursor to bottom
  :normal 1GG
" Delete empty trailing line
  :normal dd
endfunction
noremap <leader>gR :call ShowRoutes()<cr>

noremap <leader>gg :vsplit Gemfile<cr>
noremap <leader>gr :vsplit config/routes.rb<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Command-T CONFIGURATIONS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:CommandTCancelMap=['<Esc>', '<C-c>']
" let g:CommandTAcceptSelectionSplitMap=['<C-e>', '<C-f>']
" let g:CommandTAcceptSelectionSplitMap=['<c-s>', '<c-CR>']
let g:CommandTMaxHeight=20
" silent! nnoremap <unique> <silent> <Leader>b :CommandTBuffer<CR>
map <leader>gv :CommandTFlush<cr>\|:CommandT app/views<cr>
map <leader>gc :CommandTFlush<cr>\|:CommandT app/controllers<cr>
map <leader>gm :CommandTFlush<cr>\|:CommandT app/models<cr>
map <leader>gh :CommandTFlush<cr>\|:CommandT app/helpers<cr>
map <leader>gf :CommandTFlush<cr>\|:CommandT config<cr>
map <leader>gl :CommandTFlush<cr>\|:CommandT lib<cr>
map <leader>gp :CommandTFlush<cr>\|:CommandT public<cr>
map <leader>gj :CommandTFlush<cr>\|:CommandT public/javascripts<cr>
map <leader>gs :CommandTFlush<cr>\|:CommandT spec<cr>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" CTRLP.vim CONFIGURATIONS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" silent! nnoremap <unique> <silent> <Leader>gt :CtrlPTag<CR>
" silent! nnoremap <unique> <silent> <Leader>f :CtrlP<CR>
" silent! nnoremap <unique> <silent> <Leader>bb :CtrlPBuffer<CR>
" let g:ctrlp_match_window = 'bottom,order:ttb,min:1,max:20,results:20'
" let g:ctrlp_map = '<\-t>'
" let g:ctrlp_max_files = 0
" let g:ctrlp_working_path_mode = 0
" let g:ctrlp_extensions = [ 'ctrlp-filetpe' ]
" let g:ctrlp_follow_symlinks = 1
" let g:ctrlp_switch_buffer = 0
" let g:ctrlp_mruf_max = 0
" let g:ctrlp_mruf_relative = 1
" let g:ctrlp_prompt_mappings = {
"     \ 'AcceptSelection("h")': ['<c-x>', '<c-cr>', '<c-e>']
"     \}
" map <leader>ga :CtrlP app/assets<cr>
" map <leader>gc :CtrlP app/controllers<cr>
" map <leader>gh :CtrlP app/helpers<cr>
" map <leader>gm :CtrlP app/models<cr>
" map <leader>gv :CtrlP app/views<cr>
" map <leader>gf :CtrlP config<cr>
" map <leader>gl :CtrlP lib<cr>
" map <leader>gp :CtrlP public<cr>
" map <leader>gs :CtrlP spec<cr>

" Keymaps
let g:ctrlp_prompt_mappings = {
	\ 'PrtSelectMove("j")':   ['<c-n>', '<down>'],
	\ 'PrtSelectMove("k")':   ['<c-p>', '<up>'],
	\ 'PrtHistory(-1)':       ['<c-j>'],
	\ 'PrtHistory(1)':        ['<c-k>'],
	\ }


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Vim-Ruby-Refactoring CONFIGURATIONS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nnoremap <leader>rap  :RAddParameter<cr>
nnoremap <leader>rcpc :RConvertPostConditional<cr>
nnoremap <leader>rel  :RExtractLet<cr>
vnoremap <leader>rec  :RExtractConstant<cr>
vnoremap <leader>relv :RExtractLocalVariable<cr>
nnoremap <leader>rit  :RInlineTemp<cr>
vnoremap <leader>rrlv :RRenameLocalVariable<cr>
vnoremap <leader>rriv :RRenameInstanceVariable<cr>
vnoremap <leader>rem  :RExtractMethod<cr>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Vimux CONFIGURATIONS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Run the current file with rspec
map <Leader>rs :call VimuxRunCommand("clear; rspec " . bufname("%"))<CR>
" Prompt for a command to run map
map <Leader>vp :VimuxPromptCommand<CR>
" Run last command executed by VimuxRunCommand
map <Leader>vl :VimuxRunLastCommand<CR>
" Inspect runner pane map
map <Leader>vi :VimuxInspectRunner<CR>
" Close vim tmux runner opened by VimuxRunCommand
map <Leader>vc :VimuxCloseRunner<CR>
" Interrupt any command running in the runner pane map
map <Leader>vx :VimuxInterruptRunner<CR>
" Clear the tmux history of the runner pane
" map <Leader>vc :VimuxClearRunnerHistory<CR>


" NERDTree plugin configuration
let NERDTreeWinSize = 26
let NERDTreeAutoCenter=1
let NERDTreeChDirMode=2
let g:NERDTreeMinimalUI=1
map <F1> :NERDTreeToggle<CR>
map <ESC>[1;2P :NERDTreeFind<CR>


" ack.vim configuration
if executable("ack")
    " ,a to Ack (search in files)
    nnoremap <leader>A :Ack<space>
    vnoremap <leader>A y:Ack <c-r>" 
    function! AckOperator(type)
      echo a:type
      if a:type ==# 'char'
        let saved_reg = @@
        normal! `[v`]y
        call feedkeys(":Ack " . @@ . " ")
        let @@ = saved_reg
      else
        return
      end
    endfunction
    nnoremap <silent> <leader>a :set opfunc=AckOperator<CR>g@

    let g:ackprg="ack -H --smart-case --nocolor --nogroup --column --ignore-dir=.binstubs --ignore-dir=vendor --ignore-dir=log --ignore-dir=tags --ignore-dir=tmp --ignore-file=is:Session.vim --ignore-file=is:tags"
    let g:ackhighlight=1
endif
nnoremap + :cn<cr>
nnoremap _ :cp<cr>


" vim-javascript configuration
let g:html_indent_inctags = "html,body,head,tbody"
let g:html_indent_script1 = "inc"
let g:html_indent_style1 = "inc"

" vim-ruby config
:let g:ruby_indent_access_modifier_style = 'indent'

" filetype detection
autocmd BufNewFile,BufRead Thorfile set filetype=ruby
autocmd BufNewFile,BufRead *.thor set filetype=ruby
autocmd BufNewFile,BufRead Gemfile set filetype=ruby
autocmd BufNewFile,BufRead Capfile set filetype=ruby
autocmd BufNewFile,BufRead pryrc set filetype=ruby
autocmd BufNewFile,BufRead *.less set filetype=css
autocmd BufNewFile,BufRead *.god set filetype=ruby
autocmd BufNewFile,BufRead *.mkd set ai formatoptions=tcroqn2 comments=n:>
autocmd BufNewFile,BufRead *.coffee set filetype=coffee
autocmd Filetype gitcommit setlocal textwidth=78
autocmd FileType gitcommit call setpos('.', [0, 1, 1, 0])
autocmd FileType go autocmd BufWritePre <buffer> Fmt
autocmd FileType go,c,rust set ts=4 sw=4 sts=4 et

set mouse=a

cnoremap vh vert leftabove help 
inoremap <c-c> <ESC>
nnoremap <c-c> <ESC>
nnoremap <c-x><c-c> <ESC>:qa<CR>
inoremap <c-x><c-c> <ESC>:qa<CR>
noremap <c-x><c-x> <ESC>:qa!<CR>
inoremap <c-x><c-x> <ESC>:qa!<CR>
noremap <c-x><c-s> <ESC>:w<CR>
inoremap <c-x><c-s> <c-o>:w<CR>
snoremap <c-x><c-s> <c-o>:w<CR>
noremap <c-x><c-k> :bw!<CR>
inoremap <c-x><c-k> <ESC>:bw!<CR>

" noremap <c-x><c-b> <ESC>:CommandTBuffer<CR>
" inoremap <c-x><c-b> <ESC>:CommandTBuffer<CR>
" noremap <c-x>b <ESC>:CommandTMRU<CR>
" inoremap <c-x>b <ESC>:CommandTMRU<CR>
" noremap <c-x><c-f> <ESC>:CommandT<CR>
" inoremap <c-x><c-f> <ESC>:CommandT<CR>
noremap <c-x><c-b> <ESC>:CtrlPBuffer<CR>
inoremap <c-x><c-b> <ESC>:CtrlPBuffer<CR>
noremap <c-x>b <ESC>:CtrlPMRU<CR>
inoremap <c-x>b <ESC>:CtrlPMRU<CR>
noremap <c-x><c-f> <ESC>:CtrlP<CR>
inoremap <c-x><c-f> <ESC>:CtrlP<CR>
noremap <esc><cr> o<ESC>
cnoremap <c-p> <up>
cnoremap <c-n> <down>
noremap <C-h> <C-w><Left>
inoremap <c-e> <c-o>A
inoremap <c-a> <c-o>^
inoremap <c-f> <Right>
inoremap <c-b> <Left>
inoremap <c-n> <Down>
inoremap <c-p> <Up>
inoremap <c-d> <Del>
inoremap <c-k> <c-o>C
cnoremap <c-b> <Left>
cnoremap <c-f> <Right>
cnoremap <c-a> <c-b>
if has('nvim')
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
else
  inoremap <esc>f <c-o>e
  cnoremap <esc>f <S-Right>
  inoremap <esc>b <c-o>b
  cnoremap <esc>b <S-Left>
  inoremap <esc>< <c-o>gg<c-o>0
  inoremap <esc>> <c-o>G<c-o>$

  noremap <esc>w <c-w>
  inoremap <esc>w <c-w>
  inoremap <esc><bs> <esc><esc>caw
  cnoremap <esc><bs> <c-w>
endif

nnoremap <Leader>gB <ESC>:Gblame<CR>
nnoremap <Leader>gb <ESC>:Git branch -vv<CR>
nnoremap <Leader>gll <ESC>:Git log<CR>
nnoremap <Leader>glp <ESC>:Git log -p<CR>
nnoremap <Leader>gco <ESC>:Git checkout -
nnoremap <Leader>gci <ESC>:Git commit -am ''<Left>
nnoremap <Leader>gs <ESC>:Git status<CR>
nnoremap <Leader>gdd <ESC>:Git diff<CR>
nnoremap <Leader>gdc <ESC>:Git diff --cached<CR>
nnoremap <Leader>gdt <ESC>:Git difftool<CR>
nnoremap <Leader>gps <ESC>:Git push 
nnoremap <Leader>gpl <ESC>:Git pull<CR>
imap <Nul> <Nop>
inoremap <c-^> <ESC><c-^>
if has('nvim')
  noremap <A-tab> <c-^>
  inoremap <A-tab> <ESC><c-^>
else
  noremap <ESC><TAB> <c-^>
  inoremap <ESC><TAB> <ESC><c-^>
endif
noremap <F10> <c-^>
inoremap <F10> <ESC><c-^>
if has('nvim')
  noremap <A-1> :call GotoBuffer(1)<CR>
  noremap <A-2> :call GotoBuffer(2)<CR>
  noremap <A-3> :call GotoBuffer(3)<CR>
  noremap <A-4> :call GotoBuffer(4)<CR>
  noremap <A-5> :call GotoBuffer(5)<CR>
  noremap <A-6> :call GotoBuffer(6)<CR>
  noremap <A-7> :call GotoBuffer(7)<CR>
  noremap <A-8> :call GotoBuffer(8)<CR>
  noremap <A-9> :call GotoBuffer(9)<CR>
  noremap <A-0> :call GotoBuffer("L")<CR>
  noremap <A-=> :call GotoBuffer(">")<CR>
  noremap <A--> :call GotoBuffer("<")<CR>
  noremap <A-!> :call GotoBuffer(1)<CR>
  noremap <A-@> :call GotoBuffer(2)<CR>
  noremap <A-#> :call GotoBuffer(3)<CR>
  noremap <A-$> :call GotoBuffer(4)<CR>
  noremap <A-%> :call GotoBuffer(5)<CR>
  noremap <A-^> :call GotoBuffer(6)<CR>
  noremap <A-&> :call GotoBuffer(7)<CR>
  noremap <A-*> :call GotoBuffer(8)<CR>
  noremap <A-(> :call GotoBuffer(9)<CR>
  noremap <A-)> :call GotoBuffer("L")<CR>
  noremap <A-+> :call GotoBuffer(">")<CR>
  noremap <A-_> :call GotoBuffer("<")<CR>
  noremap <A-t> :enew<CR>
  noremap <A-x> :bd<CR>
  noremap <A-X> :bd!<CR>
  inoremap <A-t> <ESC>:enew<CR>
  inoremap <A-x> <ESC>:bd<CR>
  inoremap <A-X> <ESC>:bd!<CR>
else
  noremap <ESC>1 :call GotoBuffer(1)<CR>
  noremap <ESC>2 :call GotoBuffer(2)<CR>
  noremap <ESC>3 :call GotoBuffer(3)<CR>
  noremap <ESC>4 :call GotoBuffer(4)<CR>
  noremap <ESC>5 :call GotoBuffer(5)<CR>
  noremap <ESC>6 :call GotoBuffer(6)<CR>
  noremap <ESC>7 :call GotoBuffer(7)<CR>
  noremap <ESC>8 :call GotoBuffer(8)<CR>
  noremap <ESC>9 :call GotoBuffer(9)<CR>
  noremap <ESC>0 :call GotoBuffer("L")<CR>
  noremap <ESC>= :call GotoBuffer(">")<CR>
  noremap <ESC>- :call GotoBuffer("<")<CR>
  noremap <ESC>! :call GotoBuffer(1)<CR>
  noremap <ESC>@ :call GotoBuffer(2)<CR>
  noremap <ESC># :call GotoBuffer(3)<CR>
  noremap <ESC>$ :call GotoBuffer(4)<CR>
  noremap <ESC>% :call GotoBuffer(5)<CR>
  noremap <ESC>^ :call GotoBuffer(6)<CR>
  noremap <ESC>& :call GotoBuffer(7)<CR>
  noremap <ESC>* :call GotoBuffer(8)<CR>
  noremap <ESC>( :call GotoBuffer(9)<CR>
  noremap <ESC>) :call GotoBuffer("L")<CR>
  noremap <ESC>+ :call GotoBuffer(">")<CR>
  noremap <ESC>_ :call GotoBuffer("<")<CR>
  noremap <ESC>t :enew<CR>
  noremap <ESC>x :bd<CR>
  noremap <ESC>X :bd!<CR>
  inoremap <ESC>t <ESC>:enew<CR>
  inoremap <ESC>x <ESC>:bd<CR>
  inoremap <ESC>X <ESC>:bd!<CR>
endif
cnoremap sudow w ! sudo tee %
nmap <silent> <Up> :silent! normal ddkP<CR>
nmap <Down> ddp

" Search for selected text, forwards or backwards.
vnoremap <silent> * :<C-U>
  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
  \gvy/<C-R><C-R>=substitute(
  \escape(@", '/\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
  \gV:call setreg('"', old_reg, old_regtype)<CR>
vnoremap <silent> # :<C-U>
  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
  \gvy?<C-R><C-R>=substitute(
  \escape(@", '?\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
  \gV:call setreg('"', old_reg, old_regtype)<CR>

set splitbelow
set splitright

:set guioptions-=r
:set guioptions-=R
:set guioptions-=l
:set guioptions-=L

augroup vimrc-auto-mkdir
  autocmd!
  autocmd BufWritePre * call s:auto_mkdir(expand('<afile>:p:h'), v:cmdbang)
  function! s:auto_mkdir(dir, force)
    if !isdirectory(a:dir)
          \   && (a:force
          \       || input("'" . a:dir . "' does not exist. Create? [y/N]") =~? '^y\%[es]$')
      call mkdir(iconv(a:dir, &encoding, &termencoding), 'p')
    endif
  endfunction
augroup END

autocmd FileType lisp,clojure let b:AutoClosePairs = AutoClose#DefaultPairsModified("", "'")
autocmd FileType lisp,clojure let b:AutoClosePairs = AutoClose#DefaultPairsModified("", "`")

colorscheme base16-railscasts

let g:ctrlspace_save_workspace_on_exit = 1
let g:gitguwtter_sign_column_always = 1

if has('nvim')
  inoremap <expr><A-/>  "\<C-x>\<C-u>"
else
  inoremap <expr><ESC>/  "\<C-x>\<C-u>"
endif

if has("gui_running")
    set guifont=Ubuntu\ Mono\ derivative\ Powerline:h15
endif

" let $XIKI_DIR = "/Users/liuxiang/cascode/github.com/xiki"
" source /Users/liuxiang/cascode/github.com/xiki/etc/vim/xiki.vim

iabbrev pry require 'pry'; binding.pry;<ESC>

function! GotoBeginningOf(type, ...)
  normal `[
  if a:type == "line"
    normal ^
  endif
endfunction
 
function! GotoEndOf(type, ...)
  normal `]
  if a:type == "line"
    normal $
  endif
endfunction
 
nnoremap <silent> <leader>> :set opfunc=GotoEndOf<CR>g@
nnoremap <silent> <leader>< :set opfunc=GotoBeginningOf<CR>g@
fu! CustomFoldText()
   "get first non-blank line
   let fs = v:foldstart
   while getline(fs) =~ '^\s*$' | let fs = nextnonblank(fs + 1)
   endwhile
   if fs > v:foldend
       let line = getline(v:foldstart)
   else
       let line = substitute(getline(fs), '\t', repeat(' ', &tabstop), 'g')
   endif

   let w = winwidth(0) - &foldcolumn - (&number ? 8 : 0)
   let foldSize = 1 + v:foldend - v:foldstart
   let foldSizeStr = " " . foldSize . " lines "
   let foldLevelStr = repeat("+--", v:foldlevel)
   let lineCount = line("$")
   let foldPercentage = printf("[%.1f", (foldSize*1.0)/lineCount*100) . "%] "
   let expansionString = repeat(".", w - strwidth(foldSizeStr.line.foldLevelStr.foldPercentage))
   return line . expansionString . foldSizeStr . foldPercentage . foldLevelStr
endf
set foldtext=CustomFoldText()

function! ToggleFolding()
  if &l:foldmethod == 'syntax'
    let &l:foldmethod = 'manual'
  else
    let &l:foldmethod = 'syntax'
    let &l:foldlevel = 99
  endif
endfunction

noremap <F4> :call ToggleFolding()<CR>
inoremap <F4> <c-o>:call ToggleFolding()<CR>
