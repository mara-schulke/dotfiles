" Mara Schulke
"
" https://github/schulke-214
" https://blog.schulke.xyz

set nocompatible    " required
filetype off        " required

"""""""""""""""""""""""""""""""""""""""""""
" General :: Plugins
"""""""""""""""""""""""""""""""""""""""""""

" https://github.com/jreybert/vimagit

call plug#begin('~/.local/share/nvim/plug')

" syntax support
Plug 'rust-lang/rust.vim'
Plug 'LnL7/vim-nix'
Plug 'ron-rs/ron.vim'
Plug 'vim-syntastic/syntastic'

" ui
Plug 'antoinemadec/FixCursorHold.nvim'
Plug 'lambdalisue/fern.vim'
Plug 'lambdalisue/fern-git-status.vim'
Plug 'lambdalisue/fern-mapping-git.vim'

Plug 'preservim/nerdcommenter'
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'

" ux
Plug 'neoclide/coc.nvim', { 'branch': 'release' }
Plug 'bronson/vim-visual-star-search'
Plug 'junegunn/goyo.vim'

" customization
Plug 'chriskempson/base16-vim'
Plug 'itchyny/lightline.vim'

" LaTeX
Plug 'lervag/vimtex'
"Plug 'xuhdev/vim-latex-live-preview', { 'for': 'tex' }

call plug#end()

"""""""""""""""""""""""""""""""""""""""""""
" General :: Config
"""""""""""""""""""""""""""""""""""""""""""

" core
filetype plugin indent on
set autoindent
set mouse=a
set wrap
set backspace=indent,eol,start
set clipboard+=unnamedplus
set signcolumn=yes
set relativenumber
set number
set laststatus=2
set encoding=utf-8
set list
set listchars=tab:➞\ ,extends:›,precedes:‹,nbsp:·,trail:·,space:·
set wildignore+=*.pyc,*.o,*.obj,*.svn,*.swp,*.class,*.hg,*.DS_Store,*.min.*
set noshowmode
set nowrap
set colorcolumn=80

" tabs
set tabstop=4
set shiftwidth=4
set smarttab
set expandtab

" splits
set splitright
set splitbelow

" search
set incsearch
set ignorecase
set smartcase
set gdefault

" theme
syntax on
set t_Co=256
set termguicolors
set background="dark"
let base16colorspace=256
hi Normal ctermbg=NONE
colorscheme base16-nixos
call Base16hi("Comment",     g:base16_gui09, "", g:base16_cterm09, "", "", "")
call Base16hi("CocHintSign", g:base16_gui03, "", g:base16_cterm03, "", "", "")
call Base16hi("NonText",     g:base16_gui01, "", g:base16_cterm01, "", "", "")
call Base16hi("SpecialKey",  g:base16_gui01, "", g:base16_cterm01, "", "", "")

"""""""""""""""""""""""""""""""""""""""""""
" General :: Plugin Configuration
"""""""""""""""""""""""""""""""""""""""""""

" coc
let g:coc_global_extensions = [
  \ 'coc-css',
  \ 'coc-emmet',
  \ 'coc-eslint',
  \ 'coc-git',
  \ 'coc-highlight',
  \ 'coc-html',
  \ 'coc-json',
  \ 'coc-markdownlint',
  \ 'coc-pairs',
  \ 'coc-prettier',
  \ 'coc-pyright',
  \ 'coc-rls',
  \ 'coc-java',
  \ 'coc-snippets',
  \ 'coc-tslint',
  \ 'coc-tsserver',
  \ 'coc-vimtex',
  \ ]

" fern
let g:fern#opener = 'edit'
let g:fern#default_hidden = 1
let g:fern#default_exclude = '.git/'
let g:fern#drawer_width = 40
let g:fern#disable_default_mappings = 1

" syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0
let g:syntastic_check_on_w = 0
let g:syntastic_tex_checkers = ['chktex']
"let g:syntastic_quiet_messages = { '!level': 'errors', 'type': 'style' }

" rust
let g:rust_cargo_check_all_features = 1
let g:rustfmt_autosave = 1

" telescope
lua << EOF
require('telescope').setup {
  defaults = {
    initial_mode = "insert",
    layout_strategy = "vertical",
    file_sorter =  require'telescope.sorters'.get_fzy_sorter,
    mappings = {
      i = {
        ["<esc>"] = require('telescope.actions').close
      },
    },
  },
  pickers = {
    buffers = {
      sort_lastused = true
    },
  },
}
EOF

"""""""""""""""""""""""""""""""""""""""""""
" Keybindings
"
"""""""""""""""""""""""""""""""""""""""""""

let mapleader = ' '

" plugins
map <leader>i :ToggleRustHints<CR>
map <C-e> :FernToggle<CR>

""<Plug>(coc-snippets-expand)
" vnoremap <C-y> <Plug>(coc-snippets-select)
nnoremap <leader><leader>t :call nerdcommenter#Comment('n', 'alignleft')<CR>
vnoremap <leader><leader>t :call nerdcommenter#Comment('x', 'alignleft')<CR>
nnoremap <leader>t :call nerdcommenter#Comment('n', 'toggle')<CR>
vnoremap <leader>t :call nerdcommenter#Comment('x', 'toggle')<CR>

map <C-p> :Commands<CR>
map <C-f> :Files<CR>
map <C-b> :Buffers<CR>
map <C-g> :Commits<CR>

nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
nmap <leader>qf  <Plug>(coc-fix-current)
nmap <leader>rn <Plug>(coc-rename)
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

" searching / replacing
" <C-Q> = vb
" map <Leader><Leader>
nnoremap <leader>h :nohlsearch<cr>
vnoremap <leader>h :nohlsearch<cr>

nnoremap <leader>r :s///g<Left><Left>
nnoremap <leader>rc :s///gc<Left><Left><Left>

xnoremap <leader>r :s///g<Left><Left>
xnoremap <leader>rc :s///gc<Left><Left><Left>

nnoremap <silent> * :let @/='\<'.expand('<cword>').'\>'<CR>cgn
xnoremap <silent> * "sy:let @/=@s<CR>cgn

" splits / windows
" closing windows shortcut is awful
map <C-c> :wincmd q<CR>
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
nnoremap <C-w>, :new<CR>
nnoremap <C-w>. :vnew<CR>

" files
map <C-s> :w<CR>

" editing
noremap Q gqq

nnoremap j gj
nnoremap k gk

nnoremap <A-j> :m +1<CR>
vnoremap <A-j> :m '>+1<CR>gv=gv
inoremap <A-j> <Esc>:m +1<CR>==gi

nnoremap <A-k> :m -2<CR>
vnoremap <A-k> :m '<-2<CR>gv=gv
inoremap <A-k> <Esc>:m -2<CR>==gi

" term mode
tnoremap <C-h> <C-\><C-n><C-w>h
tnoremap <C-j> <C-\><C-n><C-w>j
tnoremap <C-k> <C-\><C-n><C-w>k
tnoremap <C-l> <C-\><C-n><C-w>l

"""""""""""""""""""""""""""""""""""""""""""
" Commands
"""""""""""""""""""""""""""""""""""""""""""

command! -nargs=0 FernToggle :Fern . -drawer -toggle -keep

command! -nargs=0 EditConfig :e $MYVIMRC
command! -nargs=0 ReloadConfig :so $MYVIMRC

command! -nargs=0 Fmt :call CocAction('format') 
command! -nargs=0 OrganizeImports :call CocAction('runCommand', 'editor.action.organizeImport')
command! -nargs=0 ToggleRustHints :call CocAction('runCommand', 'rust-analyzer.toggleInlayHints')

command! -nargs=0 Files :Telescope find_files theme=get_dropdown
command! -nargs=0 Buffers :Telescope buffers theme=get_dropdown
command! -nargs=0 Commits :Telescope git_commits theme=get_dropdown
command! -nargs=0 Commands :Telescope commands theme=get_dropdown
command! -nargs=0 Branches :Telescope git_branches theme=get_dropdown
command! -nargs=0 Grep :Telescope live_grep theme=get_dropdown

"""""""""""""""""""""""""""""""""""""""""""
" Auto Commands
"""""""""""""""""""""""""""""""""""""""""""

function! SyntasticCheckHook(errors)
    if !empty(a:errors)
        let g:syntastic_loc_list_height = min([len(a:errors), 5])
    endif
endfunction

function! s:init_fern() abort
    " movement
    map <buffer> <C-LeftMouse> <LeftMouse><Plug>(fern-action-open-or-expand)
    map <buffer> <C-RightMouse> <LeftMouse><Plug>(fern-action-collapse)
    map <buffer> <CR> <Plug>(fern-action-open-or-expand)
    map <buffer> l <Plug>(fern-action-open-or-expand)
    map <buffer> h <Plug>(fern-action-collapse)
    " filesystem ops
    map <buffer> N <Plug>(fern-action-new-path)
    map <buffer> R <Plug>(fern-action-rename)
    map <buffer> M <Plug>(fern-action-move)
    map <buffer> D <Plug>(fern-action-remove)
    " misc
    map <buffer> <leader> <Plug>(fern-action-mark:toggle)
    map <buffer> r <Plug>(fern-action-reload)
endfunction

augroup fern
    autocmd! *
    autocmd FileType fern setlocal signcolumn=no
    autocmd FileType fern setlocal cc=
    autocmd FileType fern setlocal nowrap
    autocmd FileType fern call s:init_fern()
augroup END

autocmd CursorHold * silent call CocActionAsync('highlight')

autocmd BufWritePost *.plantuml call jobstart('plantuml '.expand('%'), {'detach': 1})

autocmd FileType tex setlocal noexpandtab

"""""""""""""""""""""""""""""""""""""""""""
" Todo List
"""""""""""""""""""""""""""""""""""""""""""

" Todo: Write Shortcut Overview or smth like that
" gf - open mentioned file
" gv - reselt last selection
" gJ - join without space
" g& - run prev substitute on whole file
" zz zt zb / CTRL U / CTRL D Move Page
" } / { to move between paragraphs of text
" :sort !!!
" D = d$
" :earler / :later
" dap / dip / cap / cip delete / change paragraph
" set clipboard+=unnamedplus
" ctrl v


"""""""""""""""""""""""""""""""""""""""""""
" Quick Fixes
"""""""""""""""""""""""""""""""""""""""""""

" enable lightline after config reload
call lightline#enable()
