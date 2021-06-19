" Maximilian Schulke
"
" https://github/schulke-214
" https://blog.maximilianschulke.com

set nocompatible    " required
filetype off        " required

"""""""""""""""""""""""""""""""""""""""""""
" General :: Plugins
"""""""""""""""""""""""""""""""""""""""""""

" https://github.com/jreybert/vimagit

call plug#begin('~/.local/share/nvim/plug')

" syntax support
Plug 'rust-lang/rust.vim'
Plug 'vim-syntastic/syntastic'

" ui
Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'preservim/nerdcommenter'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" ux
Plug 'neoclide/coc.nvim', { 'branch': 'release' }
Plug 'bronson/vim-visual-star-search'

" customization
Plug 'nanotech/jellybeans.vim'
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
set relativenumber
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
colorscheme jellybeans

"""""""""""""""""""""""""""""""""""""""""""
" General :: Plugin Configuration
"""""""""""""""""""""""""""""""""""""""""""

let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#buffer_nr_show = 1
let g:airline#extensions#tabline#formatter = 'unique_tail'
let g:airline_powerline_fonts = 1
let g:airline_theme='minimalist'
let g:airline_statusline_ontop=1

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
  \ 'coverage',
  \ 'dumps',
  \ 'node_modules',
  \ 'target',
  \ '__pycache__',
  \ '.git',
  \ '.idea',
  \ '.pytest_cache',
  \ '.vim'
  \ ]

let g:NERDTreeGitStatusConcealBrackets = 1
let g:NERDTreeGitStatusIndicatorMapCustom = {
  \ 'Modified'  :'*',
  \ 'Staged'    :'+',
  \ 'Untracked' :'!',
  \ 'Renamed'   :'>',
  \ 'Unmerged'  :'=',
  \ 'Deleted'   :'-',
  \ 'Dirty'     :'~',
  \ 'Ignored'   :'?',
  \ 'Clean'     :'/',
  \ 'Unknown'   :'#'
  \ }

" syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

let g:syntastic_quiet_messages = { '!level': 'errors', 'type': 'style' }

" EchoDoc
let g:echodoc#enable_at_startup = 1
let g:echodoc#type = 'floating'
let g:echodoc#events = ['CompleteDone']
highlight link EchoDocFloat Pmenu

"""""""""""""""""""""""""""""""""""""""""""
" Keybindings
"""""""""""""""""""""""""""""""""""""""""""

let mapleader = ' '

" plugins
map <C-e> :NERDTreeToggle<CR>
""<Plug>(coc-snippets-expand)
" vnoremap <C-y> <Plug>(coc-snippets-select)
nnoremap <C-t> :call NERDComment('n', 'toggle')<CR>
vnoremap <C-t> :call NERDComment('x', 'toggle')<CR>

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

nnoremap <leader>r :s///g<Left><Left>
nnoremap <leader>rc :s///gc<Left><Left><Left>

xnoremap <leader>r :s///g<Left><Left>
xnoremap <leader>rc :s///gc<Left><Left><Left>

nnoremap <silent> * :let @/='\<'.expand('<cword>').'\>'<CR>cgn
xnoremap <silent> * "sy:let @/=@s<CR>cgn

" splits / windows
" closing windows shortcut is awful
map <leader>w :wincmd q<CR>
map <leader># :b#<CR>
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" files
map <C-s> :w<CR>

" editing
noremap Q gqq

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

command! -nargs=0 EditConfig :e $MYVIMRC

command! -nargs=0 ReloadConfig :so $MYVIMRC

command! -nargs=0 Fmt :call CocAction('format') 

command! -nargs=0 OrganizeImports :call CocAction('runCommand', 'editor.action.organizeImport')

"""""""""""""""""""""""""""""""""""""""""""
" Auto Commands
"""""""""""""""""""""""""""""""""""""""""""

autocmd CursorHold * silent call CocActionAsync('highlight')
" autocmd CursorHoldI * call echodoc#enable()
" autocmd CursorMoved * call echodoc#disable()

" set sessionoptions-=help
" let workspace_session_file = '.vim/session.vim'

" autocmd VimLeave * NERDTreeClose
" autocmd VimLeave */workspace* mksession! .vim/session.vim
" autocmd VimEnter */workspace* e foo

" function! MakeSession()
"   let b:sessiondir = $HOME . "/.vim/sessions" . getcwd()
"   if (filewritable(b:sessiondir) != 2)
"     exe 'silent !mkdir -p ' b:sessiondir
"     redraw!
"   endif
"   let b:filename = b:sessiondir . '/session.vim'
"   exe "mksession! " . b:filename
" endfunction
" 
" function! LoadSession()
"   let b:sessiondir = $HOME . "/.vim/sessions" . getcwd()
"   let b:sessionfile = b:sessiondir . "/session.vim"
"   if (filereadable(b:sessionfile))
"     exe 'source ' b:sessionfile
"   else
"     echo "No session loaded."
"   endif
" endfunction
" au VimEnter */workspace/* nested :call LoadSession()
" au VimLeave */workspace/* :call MakeSession()

" autocmd VimEnter *  NERDTreeOpen

" source .vim/session.vim

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

