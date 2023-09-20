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
      vim.keymap.set('n', '<D-c><D-c>', '"+yy') -- Copy line
      vim.keymap.set('n', '<D-c>p', '"+yip') -- Copy paragraph
      vim.keymap.set('v', '<D-c>', '"+y') -- Copy
      vim.keymap.set('n', '<D-v>', '"+P') -- Paste normal mode
      vim.keymap.set('v', '<D-v>', '"+P') -- Paste visual mode
      vim.keymap.set('c', '<D-v>', '<C-R>+') -- Paste command mode
      -- vim.keymap.set('i', '<D-v>', '<ESC>l"+Pli') -- Paste insert mode
      vim.keymap.set('i', '<D-v>', '<C-o>"+P') -- Paste insert mode

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

  let g:spacevim_home_files_number = 30
  let g:spacevim_projects_cache_num = 200
  let g:multi_cursor_use_default_mapping=0
  let g:multi_cursor_start_word_key      = '<A-J>'
  let g:multi_cursor_select_all_word_key = '<A-m>'
  let g:multi_cursor_start_key           = 'g<A-J>'
  let g:multi_cursor_select_all_key      = 'g<A-m>'
  let g:multi_cursor_next_key            = '<A-J>'
  let g:multi_cursor_prev_key            = '<A-K>'
  let g:multi_cursor_skip_key            = '<A-X>'
  let g:multi_cursor_quit_key            = '<Esc>'
  let g:vim_textobj_parameter_mapping = 'a'

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

  let g:_spacevim_mappings_space.o = {'name' : '+Custom'}
  let g:_spacevim_mappings_space.o.c = {'name' : '+config files'}
  let g:_spacevim_mappings_space.o.d = {'name' : '+directories'}
  let g:_spacevim_mappings_space.o.e = {'name' : '+demo files'}
  let g:_spacevim_mappings_space.o.o = {'name' : '+org-mode files'}
  let l:directories = [
          \{"keys": ["o", "d", "A"], "path": "/Applications/", "desc": "applications-dir"},
          \{"keys": ["o", "d", "b"], "path": "~/blog/", "desc": "blog-dir"},
          \{"keys": ["o", "d", "B"], "path": "~/bin/", "desc": "bin-dir"},
          \{"keys": ["o", "d", "C"], "path": "~/Cafe/", "desc": "cafe-dir"},
          \{"keys": ["o", "d", "c"], "path": "~/.config/", "desc": "config-dir"},
          \{"keys": ["o", "d", "<C-c>"], "path": "~/cascode/", "desc": "cascode-dir"},
          \{"keys": ["o", "d", "d"], "path": "~/Downloads/", "desc": "downloads-dir"},
          \{"keys": ["o", "d", "D"], "path": "~/Documents/", "desc": "documents-dir"},
          \{"keys": ["o", "d", "f"], "path": "~/.fzf/", "desc": "fzf-dir"},
          \{"keys": ["o", "d", "g"], "path": "~/cascode/github.com/", "desc": "github-dir"},
          \{"keys": ["o", "d", "H"], "path": "/home", "desc": "home-dir"},
          \{"keys": ["o", "d", "h"], "path": "~/", "desc": "user-dir"},
          \{"keys": ["o", "d", "i"], "path": "~/Library/Mobile Documents/com~apple~CloudDocs/", "desc": "icloud-dir"},
          \{"keys": ["o", "d", "k"], "path": "~/kt/", "desc": "kt-dir"},
          \{"keys": ["o", "d", "l"], "path": "~/Library/Application Support/", "desc": "las-dir"},
          \{"keys": ["o", "d", "L"], "path": "~/Library/Preferences/", "desc": "lp-dir"},
          \{"keys": ["o", "d", "M"], "path": "~/Movies/", "desc": "movies-dir"},
          \{"keys": ["o", "d", "m"], "path": "~/Documents/materials/", "desc": "materials-dir"},
          \{"keys": ["o", "d", "s"], "path": "~/Documents/materials/scratches/", "desc": "scratch-dir"},
          \{"keys": ["o", "d", "S"], "path": "~/snips/", "desc": "snips-dir"},
          \{"keys": ["o", "d", "<C-s>"], "path": "~/Documents/materials/snippets", "desc": "snippets-dir"},
          \{"keys": ["o", "d", "e"], "path": "~/.emacs.spacemacs.d", "desc": "emacs-dir"},
          \{"keys": ["o", "d", "z"], "path": "~/.spacezsh/", "desc": "spacezsh-dir"},
          \{"keys": ["o", "d", "Z"], "path": "~/.oh-my-zsh/", "desc": "ohmyzsh-dir"},
          \{"keys": ["o", "d", "/"], "path": "/", "desc": "root-dir"},
          \{"keys": ["o", "d", "o"], "path": "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org", "desc": "org-dir"},
          \{"keys": ["o", "d", "j"], "path": "~/Documents/materials/journal", "desc": "journal-dir"},
          \{"keys": ["o", "d", "J"], "path": "~/Documents/materials/jira", "desc": "jira-dir"},
          \{"keys": ["o", "d", "n"], "path": "~/Documents/materials/notes", "desc": "notes-dir"},
          \{"keys": ["o", "d", "N"], "path": "~/Documents/evernotes", "desc": "evernotes-dir"},
          \{"keys": ["o", "d", "w"], "path": "~/Documents/materials/webclips", "desc": "webclips-dir"},
          \{"keys": ["o", "d", "v"], "path": "/Volumes", "desc": "volumes-dir"},
          \{"keys": ["o", "d", "T"], "path": "~/.tmux", "desc": "tmux-dir"},
          \{"keys": ["o", "d", "t"], "path": "~/tmp/", "desc": "tmp-dir"},
  \]
  for entry in directories
    call SpaceVim#mapping#space#def('nnoremap', entry.keys, 'edit ' . entry.path . ' | cd ' . entry.path, entry.desc, 1, 1)
  endfor

  let l:config_files = [
          \{"keys": ["o", "c", "h"], "path": "/etc/hosts", "desc": "hosts-config"},
          \{"keys": ["o", "c", "S"], "path": "/etc/sudoers", "desc": "sudoers"},
          \{"keys": ["o", "c", "z"], "path": "~/.zshrc", "desc": "zshrc"},
          \{"keys": ["o", "c", "p"], "path": "~/.pryrc", "desc": "pryrc"},
          \{"keys": ["o", "c", "l"], "path": "~/.vrl.yml", "desc": "vrl"},
          \{"keys": ["o", "c", "g"], "path": "~/.gitconfig", "desc": "git"},
          \{"keys": ["o", "c", "d"], "path": "~/.docker", "desc": "docker-config"},
          \{"keys": ["o", "c", "s"], "path": "~/.ssh/config", "desc": "ssh-config"},
          \{"keys": ["o", "c", "A"], "path": "~/.ssh/authorized_keys", "desc": "authorized_keys"},
          \{"keys": ["o", "c", "a"], "path": "/etc/ansible", "desc": "ansible-conf-dir"},
          \{"keys": ["o", "c", "t"], "path": "~/.tmux.conf", "desc": "tmux-conf"},
          \{"keys": ["o", "c", "J"], "path": "~/.jenkins-builder.yaml", "desc": "jenkins-builder"},
          \{"keys": ["o", "c", "j"], "path": "~/.ideavimrc", "desc": "ideavimrc"},
          \{"keys": ["o", "c", "n"], "path": "/usr/local/etc/nginx", "desc": "nginx"},
          \{"keys": ["o", "c", "k"], "path": "~/.kube/config", "desc": "kubectl"},
          \{"keys": ["o", "c", "K"], "path": "~/.k9s", "desc": "k9s"},
          \{"keys": ["o", "c", "9"], "path": "~/.k9s", "desc": "k9s2"},
          \{"keys": ["o", "c", "q"], "path": "~/.arql.d", "desc": "arql"},
          \{"keys": ["o", "c", "m"], "path": "~/.m2/settings.xml", "desc": "maven"},
          \{"keys": ["o", "c", "v"], "path": "~/.vimrc", "desc": "vimrc"}
  \]
  for entry in config_files
    call SpaceVim#mapping#space#def('nnoremap', entry.keys, 'edit ' . entry.path, entry.desc, 1, 1)
  endfor

  let l:demo_files = [
      \{"keys": ["o", "e", "a"], "path": "~/Documents/materials/demo/demo.art", "desc": "artist-demo"},
      \{"keys": ["o", "e", "A"], "path": "~/cascode/github.com/prog-scala/src/main/scala/lx/demo.scala", "desc": "scala-demo"},
      \{"keys": ["o", "e", "b"], "path": "~/Documents/materials/demo/big.txt", "desc": "bigtxt-demo"},
      \{"keys": ["o", "e", "B"], "path": "~/Documents/materials/demo/spring-boot-init-list.org", "desc": "spring-boot-init-list"},
      \{"keys": ["o", "e", "h"], "path": "~/Documents/materials/demo/demo-http.org", "desc": "http-demo"},
      \{"keys": ["o", "e", "l"], "path": "~/Documents/materials/demo/demo.el", "desc": "elisp-demo"},
      \{"keys": ["o", "e", "r"], "path": "~/Documents/materials/demo/demo.rb", "desc": "ruby-demo"},
      \{"keys": ["o", "e", "P"], "path": "~/Documents/materials/demo/demo.py", "desc": "python-demo"},
      \{"keys": ["o", "e", "J"], "path": "~/cascode/java/maven/simple/src/main/java/org/sonatype/mavenbook/App.java", "desc": "java-demo"},
      \{"keys": ["o", "e", "j"], "path": "~/Documents/materials/demo/demo.js", "desc": "js-demo"},
      \{"keys": ["o", "e", "s"], "path": "~/Documents/materials/demo/demo.sh", "desc": "shell-demo"},
      \{"keys": ["o", "e", "S"], "path": "~/Documents/materials/demo/demo.swift", "desc": "swift-demo"},
      \{"keys": ["o", "e", "p"], "path": "~/Documents/materials/demo/demo.pl", "desc": "perl-demo"},
      \{"keys": ["o", "e", "o"], "path": "~/Documents/materials/demo/demo.org", "desc": "org-demo"},
      \{"keys": ["o", "e", "m"], "path": "~/Documents/materials/demo/demo.md", "desc": "markdown-demo"},
      \{"keys": ["o", "e", "C"], "path": "~/Documents/materials/demo/clojure/demo/src/demo/demo.clj", "desc": "coffee-demo"},
      \{"keys": ["o", "e", "y"], "path": "~/Documents/materials/demo/demo.yaml", "desc": "yaml-demo"},
      \{"keys": ["o", "e", "H"], "path": "~/Documents/materials/demo/demo.html", "desc": "html-demo"},
      \{"keys": ["o", "e", "c"], "path": "~/Documents/materials/demo/demo.c", "desc": "c-demo"},
      \{"keys": ["o", "e", "g"], "path": "~/cascode/go/src/demo/demo.go", "desc": "go-demo"},
      \{"keys": ["o", "e", "q"], "path": "~/Documents/materials/demo/demo.sql", "desc": "sql-demo"},
      \{"keys": ["o", "e", "E"], "path": "~/Documents/materials/demo/demo.exs", "desc": "elixir-demo"},
      \{"keys": ["o", "e", "e"], "path": "~/Documents/materials/demo/demo.es", "desc": "es-demo"},
      \{"keys": ["o", "e", "k"], "path": "~/Documents/materials/demo/demo.hs", "desc": "haskell-demo"},
      \{"keys": ["o", "e", "x"], "path": "~/Documents/materials/mind-wave-chats/default.chat", "desc": "chat-demo"},
      \{"keys": ["o", "e", "t"], "path": "~/Documents/materials/demo/demo.txt", "desc": "txt-demo"},
  \]
  for entry in demo_files
    call SpaceVim#mapping#space#def('nnoremap', entry.keys, 'edit ' . entry.path, entry.desc, 1, 1)
  endfor

  let l:org_files = [
      \{"keys": ["o", "o", "b"], "path": "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/bookmarks.org", "desc": "bookmarks-org"},
      \{"keys": ["o", "o", "d"], "path": "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/tech_diary.org", "desc": "tech-diary-org"},
      \{"keys": ["o", "o", "h"], "path": "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/homebrews.org", "desc": "homebrews-org"},
      \{"keys": ["o", "o", "p"], "path": "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/projects.org", "desc": "projects-org"},
      \{"keys": ["o", "o", "l"], "path": "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/learnings.org", "desc": "learnings-org"},
      \{"keys": ["o", "o", "f"], "path": "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/life.org", "desc": "life-org"},
      \{"keys": ["o", "o", "c"], "path": "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/capture.org", "desc": "capture-org"},
      \{"keys": ["o", "o", "C"], "path": "~/Documents/materials/cheatsheets", "desc": "cheatsheet-org"},
      \{"keys": ["o", "o", "t"], "path": "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/team-tasks.org", "desc": "team-tasks-org"},
      \{"keys": ["o", "o", "k"], "path": "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/kbd-macros.org", "desc": "kbd-macros-org"},
      \{"keys": ["o", "o", "j"], "path": "~/Documents/materials/jira/recent-issues.org", "desc": "jira-org"},
      \{"keys": ["o", "o", "r"], "path": "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/rails-guides-org/rails-guides-index.org", "desc": "rails-guides-org"},
  \]
  for entry in org_files
    call SpaceVim#mapping#space#def('nnoremap', entry.keys, 'edit ' . entry.path, entry.desc, 1, 1)
  endfor


  lua <<EOF
    require'nvim-treesitter.configs'.setup {
      textobjects = {
        select = {
          enable = true,

          -- Automatically jump forward to textobj, similar to targets.vim
          lookahead = true,

          keymaps = {
            -- You can use the capture groups defined in textobjects.scm
            ["af"] = "@function.outer",
            ["if"] = "@function.inner",
            ["ac"] = "@class.outer",
            -- You can optionally set descriptions to the mappings (used in the desc parameter of
            -- nvim_buf_set_keymap) which plugins like which-key display
            ["ic"] = { query = "@class.inner", desc = "Select inner part of a class region" },
            -- You can also use captures from other query groups like `locals.scm`
            ["as"] = { query = "@scope", query_group = "locals", desc = "Select language scope" },
          },
          -- You can choose the select mode (default is charwise 'v')
          --
          -- Can also be a function which gets passed a table with the keys
          -- * query_string: eg '@function.inner'
          -- * method: eg 'v' or 'o'
          -- and should return the mode ('v', 'V', or '<c-v>') or a table
          -- mapping query_strings to modes.
          selection_modes = {
            ['@parameter.outer'] = 'v', -- charwise
            ['@function.outer'] = 'V', -- linewise
            ['@class.outer'] = '<c-v>', -- blockwise
          },
          -- If you set this to `true` (default is `false`) then any textobject is
          -- extended to include preceding or succeeding whitespace. Succeeding
          -- whitespace has priority in order to act similarly to eg the built-in
          -- `ap`.
          --
          -- Can also be a function which gets passed a table with the keys
          -- * query_string: eg '@function.inner'
          -- * selection_mode: eg 'v'
          -- and should return true of false
          include_surrounding_whitespace = true,
        },
      },
    }
EOF


endfunction
