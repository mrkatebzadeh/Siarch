let mapleader = "\<Space>"
let maplocalleader = ','
if ! filereadable(expand('~/.config/nvim/autoload/plug.vim'))
	echo "Downloading junegunn/vim-plug to manage plugins..."
	silent !mkdir -p ~/.config/nvim/autoload/
	silent !curl "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim" > ~/.config/nvim/autoload/plug.vim
endif

call plug#begin('~/.config/nvim/plugged')

Plug 'lervag/vimtex'
Plug 'liuchengxu/vim-which-key'
Plug 'tpope/vim-surround'
Plug 'scrooloose/nerdtree'
Plug 'junegunn/goyo.vim'
Plug 'jreybert/vimagit'
Plug 'vimwiki/vimwiki'
Plug 'bling/vim-airline'
Plug 'tpope/vim-commentary'
Plug 'vifm/vifm.vim'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

call plug#end()

set bg=light
set go=a
set mouse=a
set nohlsearch
set clipboard=unnamedplus

" Some basics:
	nnoremap c "_c
	set nocompatible
	filetype plugin on
	syntax on
	set encoding=utf-8
	set number relativenumber
" Tabs
	set expandtab
    	set tabstop=4
    	set shiftwidth=4
	if &listchars ==# 'eol:$'
	  set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
	endif
	set list
" Highlight extra spaces
	let b:highlightextrawhites=0
	highlight ExtraWhitespace ctermbg=darkgreen guibg=darkgreen
	function! HighlightWhitespaceToggle()
	  if(b:highlightextrawhites == 0)
	    match ExtraWhitespace /\s\+$\|\t/
	    let b:highlightextrawhites=1
	  else
	    let b:highlightextrawhites=0
	    match ExtraWhitespace //
	    set rnu
	  endif
	endfunc
	nnoremap <leader>tw :call HighlightWhitespaceToggle()<cr>
" Search
	set ignorecase
	set smartcase
	if maparg('<C-L>', 'n') ==# ''
	  nnoremap <silent> <C-L> :nohlsearch<CR><C-L>
	endif
	nmap <leader>s :%s//g<Left><Left>
" Relative numbering
	function! NumberToggle()
	  if(&relativenumber == 1)
	    set nornu
	    set number
	  else
	    set rnu
	  endif
	endfunc
	nnoremap <leader>tr :call NumberToggle()<cr>
" Enable autocompletion:
	set wildmode=longest,list,full
" Disables automatic commenting on newline:
	autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
" Which Key?
	nnoremap <silent> <leader>      :<c-u>WhichKey '<Space>'<CR>
	nnoremap <silent> <localleader> :<c-u>WhichKey  ','<CR>
" Goyo plugin makes text more readable when writing prose:
	map <leader>tf :Goyo \| set bg=light \| set linebreak<CR>

" Spell-check
	map <leader>ts :setlocal spell! spelllang=en_us<CR>

" Splits open at the bottom and right, which is non-retarded, unlike vim defaults.
	set splitbelow splitright

" Nerd tree
	map <leader>tn :NERDTreeToggle<CR>
	autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" Shortcutting split navigation, saving a keypress:
	map <C-h> <C-w>h
	map <C-j> <C-w>j
	map <C-k> <C-w>k
	map <C-l> <C-w>l

	autocmd VimEnter * nnoremap <leader>w<left>   :<C-w>h<cr>
	autocmd VimEnter * nnoremap <leader>w<down>   :<C-w>j<cr>
	autocmd VimEnter * nnoremap <leader>w<up>     :<C-w>k<cr>
	autocmd VimEnter * nnoremap <leader>w<right>  :<C-w>l<cr>
	autocmd VimEnter * nnoremap <leader>ws        :sp<cr>
	autocmd VimEnter * nnoremap <leader>wv        :vsp<cr>
	autocmd VimEnter * nnoremap <leader>wq        :quit<cr>
" Check file in shellcheck:
	map <leader>s :!clear && shellcheck %<CR>

" Open my bibliography file in split
	map <leader>b :vsp<space>$BIB<CR>
	map <leader>r :vsp<space>$REFER<CR>

" Replace all is aliased to S.
	nnoremap S :%s//g<Left><Left>

" Compile document, be it groff/LaTeX/markdown/etc.
	map <leader>c :w! \| !compiler <c-r>%<CR>

" Open corresponding .pdf/.html or preview
	map <leader>p :!opout <c-r>%<CR><CR>

" Runs a script that cleans out tex build files whenever I close out of a .tex file.
	autocmd VimLeave *.tex !texclear %

" Ensure files are read as what I want:
	let g:vimwiki_ext2syntax = {'.Rmd': 'markdown', '.rmd': 'markdown','.md': 'markdown', '.markdown': 'markdown', '.mdown': 'markdown'}
	let g:vimwiki_list = [{'path': '~/vimwiki', 'syntax': 'markdown', 'ext': '.md'}]
	autocmd BufRead,BufNewFile /tmp/calcurse*,~/.calcurse/notes/* set filetype=markdown
	autocmd BufRead,BufNewFile *.ms,*.me,*.mom,*.man set filetype=groff
	autocmd BufRead,BufNewFile *.tex set filetype=tex

" Copy selected text to system clipboard (requires gvim/nvim/vim-x11 installed):
	vnoremap <C-c> "+y
	map <C-p> "+P

" Enable Goyo by default for mutt writting
	" Goyo's width will be the line limit in mutt.
	autocmd BufRead,BufNewFile /tmp/neomutt* let g:goyo_width=80
	autocmd BufRead,BufNewFile /tmp/neomutt* :Goyo \| set bg=light

" Automatically deletes all trailing whitespace on save.
	autocmd BufWritePre * %s/\s\+$//e

" When shortcut files are updated, renew bash and vifm configs with new material:
	autocmd BufWritePost ~/.config/bmdirs,~/.config/bmfiles !shortcuts

" Run xrdb whenever Xdefaults or Xresources are updated.
	autocmd BufWritePost *Xresources,*Xdefaults !xrdb %

"""LATEX
" PDF Viewer
	let g:vimtex_view_method = 'zathura'
	let g:vimtex_view_forward_search_on_start = 0
	" Word count:
	autocmd FileType tex map <leader>w :w !detex \| wc -w<CR>

""" FzF
    " This is the default extra key bindings
    let g:fzf_action = {
      \ 'ctrl-t': 'tab split',
      \ 'ctrl-x': 'split',
      \ 'ctrl-v': 'vsplit' }

    " Default fzf layout
    " - down / up / left / right
    let g:fzf_layout = { 'down': '~40%' }

    " Enable per-command history.
    " CTRL-N and CTRL-P will be automatically bound to next-history and
    " previous-history instead of down and up. If you don't like the change,
    " explicitly bind the keys to down and up in your $FZF_DEFAULT_OPTS.
    let g:fzf_history_dir = '~/.local/share/fzf-history'

	nnoremap <leader>bb :Buffers<cr>
	nnoremap <leader>bd :bd<cr>
	nnoremap <leader>ff :GFiles<cr>
	nnoremap <leader>wL :Windows<cr>
