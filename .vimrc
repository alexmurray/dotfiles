set nocompatible

" use pathogen to manage addons in ~/.vim/bundle
call pathogen#infect()
call pathogen#helptags()

filetype on		" Enable filetype detection
filetype indent on	" Enable filetype-specific indenting
filetype plugin on	" Enable filetype-specific plugins

syntax enable		" syntax highlight
set background=dark	" light version of solarized
set t_Co=16
colorscheme solarized
set hlsearch		" search highlighting

" run ctags with required options for C/C++
nmap <C-F12> :!ctags -R --c++-kinds=+p --fields=+iaS --extra=+q .<CR>

" show tagbar with F8
nmap <F8> :TagbarToggle<CR>

