" General
setlocal cmdheight=1
setlocal colorcolumn=81
setlocal textwidth=80

" Document structuring
nnoremap ;be i\begin{(<>)}<CR><Tab><CR>\end{(<>)}<Esc>kk^
nnoremap ;s i\section{}<Return>(<>)<Esc>k$i

" Text formatting
inoremap ;b \textbf{}(<>)<Esc>T{i
inoremap ;i \textit{}(<>)<Esc>T{i
inoremap ;u \underline{}(<>)<Esc>T{i
inoremap ;e \emph{}(<>)<Esc>T{i

" Next section
noremap <silent> .. <Esc>/(<>)<Enter>"_c4l

" Enable/disable spell check
noremap <F7> <Esc>:setlocal spell spelllang=en_us,nl<CR>
noremap <F8> <Esc>:setlocal nospell<CR>

" Start spell check when vim is run
autocmd VimEnter * call feedkeys("\<F7>")
