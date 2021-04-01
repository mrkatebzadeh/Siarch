nnoremap <SPACE> <Nop>
let g:mapleader = "\<Space>"
let g:maplocalleader = ','
nnoremap <silent> <leader>      :<c-u>WhichKey '<Space>'<CR>
nnoremap <silent> <localleader> :<c-u>WhichKey  ','<CR>
vnoremap <silent> <leader> :<c-u>WhichKeyVisual '<Space>'<CR>
autocmd! FileType which_key
autocmd  FileType which_key set laststatus=0 noshowmode noruler
  \| autocmd BufLeave <buffer> set laststatus=2 noshowmode ruler

let g:which_key_map =  {}
let g:which_key_sep = '→'
let g:which_key_display_names = {'<CR>': '↵', '<TAB>': '⇆'}

let g:which_key_map.b = {
      \ 'name' : '+buffer' ,
      \ '>' : [':BufferMoveNext'        , 'move next'],
      \ '<' : [':BufferMovePrevious'    , 'move prev'],
      \ 'b' : [':Buffers'                , 'fzf-buffer'],
      \ 'd' : [':BufferClose'               , 'delete-buffer'],
      \ 'n' : [':bnext'                  , 'next-buffer'],
      \ 'p' : [':bprevious'              , 'previous-buffer'],
      \ 'B' : [':BufferPick'                , 'fzf-buffer'],
      \ }

let g:which_key_map.c = {
      \ 'name' : '+comments' ,
      \ '/' : [':CommentToggle'        , 'Comment Toggle'],
      \ }

let g:which_key_map.e = {
      \ 'name' : '+emmet' ,
      \ ',' : ['<Plug>(emmet-expand-abbr)'               , 'expand abbr'],
      \ ';' : ['<plug>(emmet-expand-word)'               , 'expand word'],
      \ 'u' : ['<plug>(emmet-update-tag)'                , 'update tag'],
      \ 'd' : ['<plug>(emmet-balance-tag-inward)'        , 'balance tag in'],
      \ 'D' : ['<plug>(emmet-balance-tag-outward)'       , 'balance tag out'],
      \ 'n' : ['<plug>(emmet-move-next)'                 , 'move next'],
      \ 'N' : ['<plug>(emmet-move-prev)'                 , 'move prev'],
      \ 'i' : ['<plug>(emmet-image-size)'                , 'image size'],
      \ '/' : ['<plug>(emmet-toggle-comment)'            , 'toggle comment'],
      \ 'j' : ['<plug>(emmet-split-join-tag)'            , 'split join tag'],
      \ 'k' : ['<plug>(emmet-remove-tag)'                , 'remove tag'],
      \ 'a' : ['<plug>(emmet-anchorize-url)'             , 'anchorize url'],
      \ 'A' : ['<plug>(emmet-anchorize-summary)'         , 'anchorize summary'],
      \ 'm' : ['<plug>(emmet-merge-lines)'               , 'merge lines'],
      \ 'c' : ['<plug>(emmet-code-pretty)'               , 'code pretty'],
      \ }

let g:which_key_map.f = {
      \ 'name' : '+files' ,
      \ 'f' : [':Telescope find_files'        , 'Files'],
      \ 'n' : [':NvimTreeToggle'    , 'Toggle Tree'],
      \ 'N' : [':NvimTreeOpen'            , 'Toggle Focus'],
      \ 'F' : [':NvimTreeFindFile'            , 'Tree Find'],
      \ 'w' : [':write'               , 'Write File'],
      \ }

let g:which_key_map.F = {
    \ 'name': '+fold',
    \ 'O' : [':set foldlevel=20'  , 'open all'],
    \ 'C' : [':set foldlevel=0'   , 'close all'],
    \ 'c' : [':foldclose'         , 'close'],
    \ 'o' : [':foldopen'          , 'open'],
    \ '1' : [':set foldlevel=1'   , 'level1'],
    \ '2' : [':set foldlevel=2'   , 'level2'],
    \ '3' : [':set foldlevel=3'   , 'level3'],
    \ '4' : [':set foldlevel=4'   , 'level4'],
    \ '5' : [':set foldlevel=5'   , 'level5'],
    \ '6' : [':set foldlevel=6'   , 'level6']
    \ }

let g:which_key_map.g = {
      \ 'name' : '+goto' ,
      \ 'd' : ['<Plug>(coc-definition)'      , 'Goto Definition'],
      \ 'y' : ['<Plug>(coc-type-definition)' , 'Goto Type Definition'],
      \ 'i' : ['<Plug>(coc-implementation)'  , 'Goto Implementation'],
      \ 'r' : ['<Plug>(coc-references)'      , 'Goto References'],
      \ }

let g:which_key_map.G = {
      \ 'name' : '+Git' ,
      \ 'a' : [':Gwrite', 'Gwrite'],
      \ 'c' : [':Gcommit', 'Gcommit'],
      \ 'sh' : [':Gpush', 'Gpush'],
      \ 'll' : [':Gpull', 'Gpull'],
      \ 's' : [':Gstatus', 'Gstatus'],
      \ 'b' : [':Gblame', 'Gblame'],
      \ 'd' : [':Gvdiff', 'Gvdiff'],
      \ 'r' : [':Gremove', 'Gremove'],
      \ 'o' : ['.Gbrowse', '.Gbrowse'],
      \ }

let g:which_key_map.s = {
      \ 'name' : '+search' ,
      \ '/' : [':Rg'        , 'Rg'],
      \ 'r' : [':Rgrep'    , 'Rgrep'],
      \ }

let g:which_key_map.S = {
      \ 'name' : '+Session' ,
      \ 'c' : [':SClose'          , 'Close Session']  ,
      \ 'd' : [':SDelete'         , 'Delete Session'] ,
      \ 'l' : [':SLoad'           , 'Load Session']     ,
      \ 's' : [':Startify'        , 'Start Page']     ,
      \ 'S' : [':SSave'           , 'Save Session']   ,
      \ }

let g:which_key_map.w = {
      \ 'name' : '+windows' ,
      \ 'w' : ['<C-W>w'     , 'other-window']          ,
      \ 'd' : ['<C-W>c'     , 'delete-window']         ,
      \ '-' : ['<C-W>s'     , 'split-window-below']    ,
      \ '|' : ['<C-W>v'     , 'split-window-right']    ,
      \ '2' : ['<C-W>v'     , 'layout-double-columns'] ,
      \ 'h' : ['<C-W>h'     , 'window-left']           ,
      \ 'j' : ['<C-W>j'     , 'window-below']          ,
      \ 'l' : ['<C-W>l'     , 'window-right']          ,
      \ 'k' : ['<C-W>k'     , 'window-up']             ,
      \ 'H' : ['<C-W>5<'    , 'expand-window-left']    ,
      \ 'J' : [':resize +5'  , 'expand-window-below']   ,
      \ 'L' : ['<C-W>5>'    , 'expand-window-right']   ,
      \ 'K' : [':resize -5'  , 'expand-window-up']      ,
      \ '=' : ['<C-W>='     , 'balance-window']        ,
      \ 's' : ['<C-W>s'     , 'split-window-below']    ,
      \ 'v' : ['<C-W>v'     , 'split-window-below']    ,
      \ '?' : ['Windows'    , 'fzf-window']            ,
      \ }

let g:which_key_map.t = {
      \ 'name' : '+toggles' ,
      \ 'b' : [':GitBlameToggle'        , 'GitBlame'],
      \ 't' : [':NERDTreeToggle'        , 'Tagbar'],
      \ 'n' : [':set number! relativenumber!'    , 'RelativeNumer'],
      \ }

let g:which_key_map.v = {
      \ 'name' : '+vimwiki' ,
      \ 'w' : ['<Plug>VimwikiIndex', 'VimwikiIndex'],
      \ 't' : ['<Plug>VimwikiTabIndex', 'VimwikiTabIndex'],
      \ 's' : ['<Plug>VimwikiUISelect', 'VimwikiUISelect'],
      \ 'i' : ['<Plug>VimwikiDiaryIndex', 'VimwikiDiaryIndex'],
      \ 'I' : ['<Plug>VimwikiDiaryGenerateLinks', 'VimwikiDiaryGenerateLinks'],
      \ 'W' : ['<Plug>VimwikiMakeDiaryNote', 'VimwikiMakeDiaryNote'],
      \ 'T' : ['<Plug>VimwikiTabMakeDiaryNote', 'VimwikiTabMakeDiaryNote'],
      \ 'y' : ['<Plug>VimwikiMakeYesterdayDiaryNote', 'VimwikiMakeYesterdayDiaryNote'],
      \ 'm' : ['<Plug>VimwikiMakeTomorrowDiaryNote', 'VimwikiMakeTomorrowDiaryNote'],
      \}

let g:which_key_map.l = {
      \ 'name' : '+lsp' ,
      \ 'a' : [':Lspsaga code_action'                , 'code action'],
      \ 'A' : [':Lspsaga range_code_action'          , 'selected action'],
      \ 'd' : [':Telescope lsp_document_diagnostics' , 'document diagnostics'],
      \ 'D' : [':Telescope lsp_workspace_diagnostics', 'workspace diagnostics'],
      \ 'f' : [':LspFormatting'                      , 'format'],
      \ 'I' : [':LspInfo'                            , 'lsp info'],
      \ 'v' : [':LspVirtualTextToggle'               , 'lsp toggle virtual text'],
      \ 'l' : [':Lspsaga lsp_finder'                 , 'lsp finder'],
      \ 'L' : [':Lspsaga show_line_diagnostics'      , 'line_diagnostics'],
      \ 'p' : [':Lspsaga preview_definition'         , 'preview definition'],
      \ 'q' : [':Telescope quickfix'                 , 'quickfix'],
      \ 'r' : [':Lspsaga rename'                     , 'rename'],
      \ 'T' : [':LspTypeDefinition'                  , 'type defintion'],
      \ 'x' : [':cclose'                             , 'close quickfix'],
      \ 's' : [':Telescope lsp_document_symbols'     , 'document symbols'],
      \ 'S' : [':Telescope lsp_workspace_symbols'    , 'workspace symbols'],
      \ }

let g:which_key_map.T = {
      \ 'name' : '+terminal' ,
      \ ';' : [':FloatermNew --wintype=normal --height=6'       , 'terminal'],
      \ 'f' : [':FloatermNew fzf'                               , 'fzf'],
      \ 'g' : [':FloatermNew lazygit'                           , 'git'],
      \ 'd' : [':FloatermNew lazydocker'                        , 'docker'],
      \ 'n' : [':FloatermNew node'                              , 'node'],
      \ 'N' : [':FloatermNew lf'                                , 'lf'],
      \ 'p' : [':FloatermNew python'                            , 'python'],
      \ 't' : [':FloatermToggle'                                , 'toggle'],
      \ 'h' : [':FloatermNew htop'                              , 'htop'],
      \ 'u' : [':FloatermNew ncdu'                              , 'ncdu'],
      \ }
call which_key#register('<Space>', "g:which_key_map")
