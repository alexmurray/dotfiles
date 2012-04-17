set nocompatible

" use pathogen to manage addons in ~/.vim/bundle
call pathogen#infect()
call pathogen#helptags()

filetype on		" Enable filetype detection
filetype indent on	" Enable filetype-specific indenting
filetype plugin on	" Enable filetype-specific plugins

" define DiffOrig command from example vimrc in help to diff buffer with file
" on disk
command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis | wincmd p | diffthis

" Visually select the text that was last edited/pasted (cf. gv)
nmap gV `[v`]

" Toggle spell checking with ,s
nmap <silent> <leader>s :set spell!<CR>

" set region to Australian English
set spelllang=en_au

" Influence how Vim formats text. Options described in :help fo-table
set formatoptions=croq

" Make tab smarter
set smarttab
set backspace=eol,start,indent

" case insensitive search
set ignorecase
set smartcase

" Wrap lines at 79 chars
set textwidth=79

" set terminal title based on filename etc
set title

" C coding
let c_space_errors=1

" set username for changelog mode
let g:changelog_username='Alex Murray <murray.alex@gmail.com>'

if has("autocmd")
	" use custom indent etc for Makefiles
	autocmd BufEnter ?Makefile* setlocal noet ts=8 sw=8 lcs=tab:>-,trail:x nocindent
	" set javacomplete as omnicomplete for Java files
	autocmd Filetype java setlocal omnifunc=javacomplete#Complete
endif

syntax enable		" syntax highlight
set background=dark	" light version of solarized
set t_Co=16
colorscheme solarized
set hlsearch		" search highlighting

" run ctags with required options for C/C++
nmap <C-F12> :!ctags -R --c++-kinds=+p --fields=+iaS --extra=+q .<CR>

" show tagbar with F8
nmap <F8> :TagbarToggle<CR>

