"""""""""""""""""""""""""""""""""""""""""""
""""""""""" github/schulke-214 """"""""""""
"""""""""""""""""""""""""""""""""""""""""""

set nocompatible		" required
filetype off			" required

""""""""""""""""" vundle """"""""""""""""""

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" core 
Plugin 'VundleVim/Vundle.vim'

" ux
Plugin 'scrooloose/nerdtree'
Plugin 'majutsushi/tagbar'
Plugin 'ervandew/supertab'
Plugin 'wesQ3/vim-windowswap'
Plugin 'SirVer/ultisnips'
Plugin 'junegunn/fzf.vim'
Plugin 'junegunn/fzf'
Plugin 'godlygeek/tabular'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'benmills/vimux'
Plugin 'jeetsukumaran/vim-buffergator'
Plugin 'gilsondev/searchtasks.vim'
Plugin 'Shougo/neocomplete.vim'
Plugin 'tpope/vim-dispatch'
Plugin 'jceb/vim-orgmode'
Plugin 'tpope/vim-speeddating'

" generic code support
Plugin 'jakedouglas/exuberant-ctags'
Plugin 'honza/vim-snippets'
Plugin 'Townk/vim-autoclose'
Plugin 'tomtom/tcomment_vim'
Plugin 'tobyS/vmustache'
Plugin 'janko-m/vim-test'
Plugin 'maksimr/vim-jsbeautify'
Plugin 'vim-syntastic/syntastic'
Plugin 'neomake/neomake'

" markdown / writing
Plugin 'reedes/vim-pencil'
Plugin 'tpope/vim-markdown'
Plugin 'jtratner/vim-flavored-markdown'
Plugin 'LanguageTool'

" elm
Plugin 'lambdatoast/elm.vim'

" customization 
Plugin 'AnsiEsc.vim'
Plugin 'ryanoasis/vim-devicons'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'sjl/badwolf'
Plugin 'tomasr/molokai'
Plugin 'morhetz/gruvbox'
Plugin 'zenorocha/dracula-theme', {'rtp': 'vim/'}
Plugin 'junegunn/limelight.vim'
Plugin 'romainl/Apprentice'
Plugin 'Lokaltog/vim-distinguished'
Plugin 'chriskempson/base16-vim'
Plugin 'w0ng/vim-hybrid'
Plugin 'AlessandroYorba/Sierra'
Plugin 'daylerees/colour-schemes'
Plugin 'effkay/argonaut.vim'
Plugin 'ajh17/Spacegray.vim'
Plugin 'atelierbram/Base2Tone-vim'
Plugin 'colepeters/spacemacs-theme.vim'
Plugin 'dylanaraps/wal.vim'
Plugin 'challenger-deep-theme/vim', { 'as': 'challenger-deep' }

call vundle#end()            " required
filetype plugin indent on    " required


""""""""""""""""" config """"""""""""""""""

" ressources:
" - https://github.com/amacgregor/dot-files/blob/master/vimrc
" - https://dev.to/allanmacgregor/vim-is-the-perfect-ide-e80
