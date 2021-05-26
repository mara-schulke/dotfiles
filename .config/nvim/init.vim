" Maximilian Schulke
"
" https://github/schulke-214
" https://blog.maximilianschulke.com

set nocompatible    " required
filetype off        " required

"""""""""""""""""""""""""""""""""""""""""""
" General :: Plugins
"""""""""""""""""""""""""""""""""""""""""""

call plug#begin('~/.local/share/nvim/plug')

" syntax support
Plug 'rust-lang/rust.vim'
Plug 'vim-syntastic/syntastic'

" ux
Plug 'neoclide/coc.nvim', { 'branch': 'release' }
Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'bronson/vim-visual-star-search'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" customization
Plug 'doums/darcula'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" LaTeX
Plug 'lervag/vimtex'
Plug 'xuhdev/vim-latex-live-preview', { 'for': 'tex' }

call plug#end()

"""""""""""""""""""""""""""""""""""""""""""
" General :: Config
"""""""""""""""""""""""""""""""""""""""""""

" core
set mouse=a
set nowrap
set backspace=indent,eol,start
set clipboard+=unnamedplus
set number
set laststatus=2
set encoding=utf-8
set list
set listchars=tab:➞\ ,extends:›,precedes:‹,nbsp:·,trail:·,space:·
set wildignore+=*.pyc,*.o,*.obj,*.svn,*.swp,*.class,*.hg,*.DS_Store,*.min.*

autocmd ColorScheme * highlight SpecialKey ctermfg=darkgray
autocmd ColorScheme * highlight NonText ctermfg=darkgray

" tabs
set tabstop=4
set shiftwidth=4
set smarttab
set noexpandtab

" theme
syntax on
set t_Co=256
colorscheme darcula

"""""""""""""""""""""""""""""""""""""""""""
" General :: Plugin Configuration
"""""""""""""""""""""""""""""""""""""""""""

let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#buffer_nr_show = 1
let g:airline_powerline_fonts = 1
let g:airline_theme='minimalist'

" coc
let g:coc_global_extensions = [
  \ 'coc-css',
  \ 'coc-emmet',
  \ 'coc-eslint', 
  \ 'coc-git',
  \ 'coc-highlight',
  \ 'coc-html',
  \ 'coc-json', 
  \ 'coc-pairs',
  \ 'coc-prettier', 
  \ 'coc-pyright',
  \ 'coc-rls',
  \ 'coc-snippets',
  \ 'coc-tslint',
  \ 'coc-tsserver',
  \ 'coc-vimtex',
  \ ]

" nerdtree
let g:NERDTreeRespectWildIgnore=1          " Hide the files that match wildignore
let g:NERDTreeShowHidden=1                 " Show hidden files
let g:NERDTreeWinSize=40                   " Set the window width of NERDTree
let g:NERDSpaceDelims = 1                  " Add spaces after comment delimiters by default
let g:NERDCompactSexyComs = 1              " Use compact syntax for prettified multi-line comments
let g:NERDCommentEmptyLines = 1            " Allow commenting and inverting empty lines (useful when commenting a region)
let g:NERDTrimTrailingWhitespace = 1       " Enable trimming of trailing whitespace when uncommenting
let g:NERDTreeDirArrowExpandable = '+'     " Set the expand icon
let g:NERDTreeDirArrowCollapsible = '-'    " Set the collapse icon
let g:NERDTreeIgnore=[
  \ 'node_modules',
  \ '.git'
  \ ]


"""""""""""""""""""""""""""""""""""""""""""
" Keybindings
"""""""""""""""""""""""""""""""""""""""""""

let mapleader = ' '

" plugins
map <C-e> :NERDTreeToggle<CR>
map <C-l> <Plug>(coc-snippets-expand)
vmap <C-j> <Plug>(coc-snippets-select)

map <C-f> :Files<CR>
map <C-b> :Buffers<CR>
map <C-g> :Commits<CR>

" searching / replacing
" map <Leader><Leader>

nnoremap <Leader>r :s///g<Left><Left>
nnoremap <Leader>rc :s///gc<Left><Left><Left>

xnoremap <Leader>r :s///g<Left><Left>
xnoremap <Leader>rc :s///gc<Left><Left><Left>

nnoremap <silent> * :let @/='\<'.expand('<cword>').'\>'<CR>cgn
xnoremap <silent> * "sy:let @/=@s<CR>cgn

" splits / windows
" closing windows shortcut is awful
map <Leader>w :wincmd q<CR>
map <Leader># :b#<CR>
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" files
map <C-s> :w<CR>

" editing
noremap Q gqq

map <Up> <Nop>
map <Down> <Nop>
map <Left> <Nop>
map <Right> <Nop>

nnoremap <A-j> :m +1<CR>
vnoremap <A-j> :m '>+1<CR>gv=gv
inoremap <A-j> <Esc>:m +1<CR>==gi

nnoremap <A-k> :m -2<CR>
vnoremap <A-k> :m '<-2<CR>gv=gv
inoremap <A-k> <Esc>:m -2<CR>==gi

"""""""""""""""""""""""""""""""""""""""""""
" Commands
"""""""""""""""""""""""""""""""""""""""""""

if !exists(":EditConfig")
	command EditConfig e $MYVIMRC
endif

if !exists(":ReloadConfig")
	command ReloadConfig so $MYVIMRC
endif

if !exists(":Fmt")
	command Fmt norm <Plug>(coc-format)
endif

"""""""""""""""""""""""""""""""""""""""""""
" Todo List
"""""""""""""""""""""""""""""""""""""""""""

" Todo: Write Shortcut Overview or smth like that
" gqq - formats paragraph nicely c:
" gf - open mentioned file
" gv - reselt last selection
" J - join lines
" gJ - join without space
" g& - run prev substitute on whole file!!!!
" remap capslock to escape 
" $ setxkbmap -option caps:super -variant altgr-intl
" increase xrate for key repeat
" $ xset r rate 300 50
"
" zz zt zb / CTRL U / CTRL D Move Page
" } / { to move between paragraphs of text
" :sort !!!

" CTRL R Redo stuff
" D = d$

" :earler / :later
" dap / dip / cap / cip delete / change paragraph
" set clipboard+=unnamedplus
" ctrl v

" autocommands

autocmd BufWritePost *.plantuml call jobstart('plantuml '.expand('%'), {'detach': 1})

