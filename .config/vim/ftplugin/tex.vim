" General
setlocal cmdheight=1
setlocal colorcolumn=81
setlocal textwidth=80

" Document structuring
nnoremap ;be i\begin{(<>)}<Return><Tab><Return>\end{(<>)}<Esc>kk^
nnoremap ;s i\section{}<Return>(<>)<Esc>k$i

" Text formatting
inoremap ;b \textbf{}(<>)<Esc>T{i
inoremap ;i \textit{}(<>)<Esc>T{i
inoremap ;u \underline{}(<>)<Esc>T{i
inoremap ;e \emph{}(<>)<Esc>T{i

" Next section
noremap <silent> .. <Esc>/(<>)<Enter>"_c4l
