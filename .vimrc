if &compatible
    set nocompatible
endif

" remap leader to , as is easier to reach than \
let mapleader = ","
let g:mapleader = ","

" load plugins which ship with vim
runtime ftplugin/man.vim
runtime macros/matchit.vim

" use pathogen for managing plugins
call pathogen#infect()
call pathogen#helptags()

filetype plugin indent on       " Enable filetype detection and load plugins and indenting

" display settings
set nowrap                      " don't wrap lines
set showmatch                   " show matching bracket
set matchtime=2                 " show matching bracket for 0.2 seconds
set matchpairs+=<:>             " specially for html
set showmode                    " show mode in stats bar
set showcmd                     " show typed command in status bar
set title                       " set terminal title based on filename etc
set wildmenu                    " completion with menu
set wildignore=*.o,*.obj,*.bak,*.exe,*.py[co],*.swp,*~,*.pyc,.svn
set laststatus=2                " Always show the statusline
set ruler                       " always show current position
set lazyredraw                  " don't redraw while executing macros

" editor settings
set gdefault                    " make substitutions on a line global by default
set ignorecase                  " case insensitive searching
set smartcase                   " but become sensitive if using uppercase characters
set autoindent                  " smart auto indenting
set smartindent
set smarttab                    " smart tab handling for indenting
set magic                       " change the way backslashes are used in search patterns
set backspace=indent,eol,start  " Allow backspacing over everything in insert mode
set shiftwidth=4                " spaces for autoindents
set softtabstop=4               " number of spaces a tab counts for
set expandtab                   " turn a tabs into spaces
set list                        " show tabs and trailing spaces at eol
set listchars=tab:▸\ ,trail:·
set textwidth=79                " wrap lines at 79 chars
set spelllang=en_au             " set region to Australian English
set autoread                    " automatically reload file when chaged

set hidden                      " remember undo after quitting

set spell                       " enable spell checking by default

" Influence how Vim formats text. Options described in :help fo-table
set formatoptions=croq

" filetype specific customisations
if has("autocmd")
    " Treat .json files as .js
    autocmd BufNewFile,BufRead *.json setfiletype json syntax=javascript
    " use custom indent etc for Makefiles
    autocmd FileType make setlocal noet ts=7 sw=8 lcs=tab:>-,trail:x nocindent
    " indent c etc correctly
    autocmd FileType c,cpp,java,javascript setlocal sw=2 sts=2 cin cino+=l1,(0,w1
    autocmd FileType java setlocal sw=4 sts=4 cino+=j1
    autocmd FileType javascript setlocal cino+=J1
    " Decode java build errors properly
    autocmd FileType java setlocal errorformat=%A\ %#[javac]\ %f:%l:\ %m,%-Z\ %#[javac]\ %p^,%-C%.%#
    " use ant for building java and find build.xml and build debug install
    " target
    autocmd FileType java setlocal makeprg=ant\ -find\ build.xml\ debug\ install
endif

" define DiffOrig command from example vimrc in help to diff buffer with file
" on disk
command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis | wincmd p | diffthis

" Strip trailing whitespace (,ss)
function! StripWhitespace()
    let save_cursor = getpos(".")
    let old_query = getreg('/')
    :%s/\s\+$//e
    call setpos('.', save_cursor)
    call setreg('/', old_query)
endfunction
noremap <leader>ss :call StripWhitespace()<CR>

" Save a file as root (,W)
nnoremap <leader>W :w !sudo tee % > /dev/null<CR>

" Visually select the text that was last edited/pasted (cf. gv)
nmap gV `[v`]

" Toggle spell checking with ,s
nmap <silent> <leader>s :set spell!<CR>

" show tagbar with F8
nmap <F8> :TagbarToggle<CR>

" make ctrlp ignore .git
let g:ctrlp_custom_ignore = '\.git$'

" show trailing whitespace etc for c
let c_space_errors=1

" set username for changelog mode
let g:changelog_username='Alex Murray <murray.alex@gmail.com>'

" enforce use of hjkl instead of arrow keys
nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>

" make movement work on screen lines, not actual file lines
nnoremap j gj
nnoremap k gk

" color settings (if terminal/gui supports it)
if &t_Co > 2 || has("gui_running")
    set background=dark         " light version of solarized
    set t_Co=256
    colorscheme solarized
    set hlsearch                " search highlighting
    set incsearch               " highlight search while typing
    " clear search highlighting with ,<space>
    nnoremap <leader><space> :nohlsearch<cr>
endif

" gui specific settings
if has("gui_running")
    set guifont="Ubuntu Mono 12"
    set background=light    " need to use light for gui??
    set guioptions=aA       " put selections into X clipboard
    set guioptions=f        " don't fork from shell
    set guioptions-=m       " disable menubar
    set guioptions-=r       " disable right scrollbar
    set guioptions-=T       " disable toolbar
endif
