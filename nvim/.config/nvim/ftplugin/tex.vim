if exists("b:did_user_ftplugin") | finish | endif
let b:did_user_ftplugin = 1
let g:tex_flavor = "latex"
" If it's in opt/
packadd! vimtex
call vimtex#init()
