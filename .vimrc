if &compatible
	set nocompatible
endif

" use pathogen for managing plugins
call pathogen#infect()
call pathogen#helptags()

" display settings
set nowrap 		" don't wrap lines
set showmatch		" show matching bracket
set matchtime=2         " show matching bracket for 0.2 seconds
set matchpairs+=<:>     " specially for html
set showmode		" show mode in stats bar
set showcmd		" show typed command in status bar
set title		" set terminal title based on filename etc
set wildmenu		" completion with menu
set wildignore=*.o,*.obj,*.bak,*.exe,*.py[co],*.swp,*~,*.pyc,.svn
set laststatus=2   " Always show the statusline

" editor settings
set ignorecase		" case insensitive searching
set smartcase		" but become sensitive if using uppercase characters
set smartindent		" smart auto indenting
set smarttab 		" smart tab handling for indenting
set magic               " change the way backslashes are used in search patterns
set backspace=indent,eol,start " Allow backspacing over everything in insert mode
set tabstop=4           " number of spaces a tab counts for
set shiftwidth=4        " spaces for autoindents
set expandtab           " turn a tabs into spaces
set textwidth=79 	" wrap lines at 79 chars
set spelllang=en_au	" set region to Australian English

set hidden              " remember undo after quitting

" Influence how Vim formats text. Options described in :help fo-table
set formatoptions=croq

filetype on		" Enable filetype detection
filetype indent on	" Enable filetype-specific indenting
filetype plugin on	" Enable filetype-specific plugins

" mode specific customisations
if has("autocmd")
	" use custom indent etc for Makefiles
	autocmd BufEnter ?Makefile* setlocal noet ts=8 sw=8 lcs=tab:>-,trail:x nocindent
	" set javacomplete as omnicomplete for Java files
	autocmd Filetype java setlocal omnifunc=javacomplete#Complete
endif

" define DiffOrig command from example vimrc in help to diff buffer with file
" on disk
command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis | wincmd p | diffthis

" Visually select the text that was last edited/pasted (cf. gv)
nmap gV `[v`]

" Toggle spell checking with ,s
nmap <silent> <leader>s :set spell!<CR>

" run ctags with required options for C/C++
nmap <C-F12> :!ctags -R --c++-kinds=+p --fields=+iaS --extra=+q .<CR>

" show tagbar with F8
nmap <F8> :TagbarToggle<CR>

" show trailing whitespace etc for c
let c_space_errors=1

" set username for changelog mode
let g:changelog_username='Alex Murray <murray.alex@gmail.com>'

" color settings (if terminal/gui supports it)
if &t_Co > 2 || has("gui_running")
	set background=dark	" light version of solarized
	set t_Co=256
	colorscheme solarized
	set hlsearch		" search highlighting
	set incsearch		" highlight search while typing
endif
