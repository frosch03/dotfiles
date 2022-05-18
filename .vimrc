syntax on

"call pathogen#infect()
"call pathogen#runtime_append_all_bundles()
"call pathogen#helptags()

let no_haskell_syntax_checker='true'

runtime ftplugin/man.vim
runtime ftplugin/vsutil.vim
runtime ftplugin/VimRegEx.vim

filetype plugin indent on
 
" latex-suite stuff 
" This invokes the latex-suite, when a .tex file is opend
" filetype plugin on
set grepprg=grep\ -nH\ $*
let g:tex_flavor='latex'

" let vim have access to the system's clipboard
set clipboard+=unnamed


"" stuff to fix problems with backspace for mutt
"" while starting mutt via xterm -class mutt -e mutt

set nocompatible
set term=xterm

set hls
set autoindent
set backspace=2

set tabstop=4
set shiftwidth=4
set number

set showmatch
set matchpairs=(:),[:],{:}

set laststatus=2
set statusline=%<%F%h%m%r%h%w%y\ %{&ff}\ %{strftime(\"%d/%m/%Y-%H:%M\")}%=\ %c%V\ %l\,%L\ %P

set textwidth=140
set wrap

set expandtab

set mouse=a

set printdevice=SharpMX-3500N

"set t_Co=256
colorscheme elflord
set background=dark

map <C-H> <C-W>h
map <C-L> <C-W>l
map <C-P> :hardcopy <CR>

map <silent> <F2> :call ToggleMouse()<CR>
map <silent> <F3> :set number!<CR>
map <F4>  :w!<CR>:!make <CR><CR>

" map <F5>  :w!<CR>:!/home/frosch03/.cabal/bin/haddock --html %<CR><CR>
map <F8>  :w!<CR>:!latex %<CR><CR>

map <F9>  :w!<CR>:!pdflatex %<CR><CR>
map <F10> :setlocal spell! spelllang=de_de<CR>
map <F11> :setlocal spell! spelllang=en_us<CR>
map <F12> :set mouse+=a<CR>:set map <F12> :set mouse-=a<CR>

map <C-N> :let @/=""<CR>


imap <C-H> <Left>
imap <C-L> <Right>
imap <C-J> <Down>
imap <C-K> <Up>
imap <C-E> <Esc>


"autocmd Filetype tex source ~/.vim/auctex.vim

"autocmd Filetype tex :iab listing \begin{lstlisting}[]\end{lstlisting}
"autocmd Filetype tex :iab itemize \begin{itemize}<++><++>\end{itemize}<++>

"iab lstlisting \begin{lstlisting}[caption={}, label={}]\end{lstlisting}
"iab center \begin{center}\end{center}
"iab tabular \begin{tabular}{rcl}\end{tabular}

"colorscheme evening

" Limit the width of a line to 72 chars only with the use of mutt
au BufRead /tmp/mutt-* set tw=72


" use ghc functinality for haskell files
" au Bufenter *.hs compiler ghc 
" au Bufenter *.lhs compiler ghc 

let g:ghc="/usr/bin/ghc"
"let g:optis=" -w -XTypeSynonymInstances -XFlexibleContexts" "

" configure browser for haskell_doc.vim
" let g:haddock_browser = "/usr/bin/chromium"

"neuen dateityp hinzufügen:
"au BufNewFile,BufRead *.zcml setf xml 

"Agda file type 
au BufNewFile,BufRead *.agda setf agda

function ToggleMouse()
    if &mouse =~? 'a'
        set mouse=
    else
        set mouse=a
    endif
endfunction


" Update time and date in the Row beginning with
" Date: 
" and only if it is a hlog file (for blogging)
function! LastModified()
  if &modified
    normal ms
    let n = min([20, line("$")])
    exe '1,' . n . 's#^\(Date: \).*#\1' .
          \ strftime('%F-%H:%M') . '#e'
    normal `s
  endif
endfun


autocmd BufWritePre *.hlog call LastModified()
au BufNewFile *.hlog 0r ~/.vim/hlog.skel
au BufNewFile,BufRead *.hlog set tw=72
au BufNewFile,BufRead *.hlog set filetype=hlog

autocmd Filetype *.hs source ~/.vim/haskell.vim


" select visually, press ctrl+r to copy that into a regex
vnoremap <C-r> "hy:%s/\(<C-r>h\)//gc<left><left><left>
