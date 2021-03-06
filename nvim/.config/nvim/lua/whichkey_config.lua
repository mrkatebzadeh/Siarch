local wk = require('whichkey_setup')

local keymap = {
	b = {
		name = '+buffer',
        b = {':Buffers<CR>'                , 'fzf-buffer'},
        d = {':BufferClose<CR>'               , 'delete-buffer'},
        n = {':bnext<CR>'                  , 'next-buffer'},
        p = {':bprevious<CR>'              , 'previous-buffer'},
        B = {':BufferPick<CR>'                , 'fzf-buffer'},
	},
    c = {
       name = '+comments' ,
       ['/'] = {':CommentToggle<CR>'        , 'Comment Toggle'},
       },

    e = {
       name = '+emmet' ,
       [','] = {'<Plug>(emmet-expand-abbr)'               , 'expand abbr'},
       [';'] = {'<plug>(emmet-expand-word)'               , 'expand word'},
       u = {'<plug>(emmet-update-tag)'                , 'update tag'},
       d = {'<plug>(emmet-balance-tag-inward)'        , 'balance tag in'},
       D = {'<plug>(emmet-balance-tag-outward)'       , 'balance tag out'},
       n = {'<plug>(emmet-move-next)'                 , 'move next'},
       N = {'<plug>(emmet-move-prev)'                 , 'move prev'},
       i = {'<plug>(emmet-image-size)'                , 'image size'},
       ['/'] = {'<plug>(emmet-toggle-comment)'            , 'toggle comment'},
       j = {'<plug>(emmet-split-join-tag)'            , 'split join tag'},
       k = {'<plug>(emmet-remove-tag)'                , 'remove tag'},
       a = {'<plug>(emmet-anchorize-url)'             , 'anchorize url'},
       A = {'<plug>(emmet-anchorize-summary)'         , 'anchorize summary'},
       m = {'<plug>(emmet-merge-lines)'               , 'merge lines'},
       c = {'<plug>(emmet-code-pretty)'               , 'code pretty'},
       },

    f = {
       name = '+files' ,
       f = {':Telescope find_files<CR>'        , 'Files'},
       n = {':NvimTreeToggle<CR>'    , 'Toggle Tree'},
       N = {':NvimTreeOpen<CR>'            , 'Toggle Focus'},
       F = {':NvimTreeFindFile<CR>'            , 'Tree Find'},
       w = {':write<CR>'               , 'Write File'},
       e = {':e'               , 'e'},
       m = {':Move'               , 'Move'},
       d = {':Delete<CR>'               , 'Delete'},
       r = {':Rename'               , 'Rename'},
       c = {':Chmod'               , 'Chmod'},
       k = {':Mkdir'               , 'Mkdir'},
       W = {':SudoWrite<CR>'               , 'SudoWrite'},
       E = {':SudoEdit'               , 'SudoEdit'},
       },

    F = {
     name= '+fold',
     O = {':set foldlevel=20<CR>'  , 'open all'},
     C = {':set foldlevel=0<CR>'   , 'close all'},
     c = {':foldclose<CR>'         , 'close'},
     o = {':foldopen<CR>'          , 'open'},
     ['1'] = {':set foldlevel=1<CR>'   , 'level1'},
     ['2'] = {':set foldlevel=2<CR>'   , 'level2'},
     ['3'] = {':set foldlevel=3<CR>'   , 'level3'},
     ['4'] = {':set foldlevel=4<CR>'   , 'level4'},
     ['5'] = {':set foldlevel=5<CR>'   , 'level5'},
     ['6'] = {':set foldlevel=6<CR>'   , 'level6'}
     },

    G = {
       name = '+Git' ,
       a = {':Gwrite<CR>', 'Gwrite'},
       c = {':Gcommit<CR>', 'Gcommit'},
       sh = {':Gpush<CR>', 'Gpush'},
       ll = {':Gpull<CR>', 'Gpull'},
       s = {':Gstatus<CR>', 'Gstatus'},
       b = {':Gblame<CR>', 'Gblame'},
       d = {':Gvdiff<CR>', 'Gvdiff'},
       r = {':Gremove<CR>', 'Gremove'},
       o = {'.Gbrowse', '.Gbrowse'},
       },

    s = {
       name = '+search' ,
       ['/'] = {':Rg<CR>'        , 'Rg'},
       r = {':Rgrep<CR>'    , 'Rgrep'},
       },

    S = {
       name = '+Session' ,
       c = {':SClose<CR>'          , 'Close Session'}  ,
       d = {':SDelete<CR>'         , 'Delete Session'} ,
       l = {':SLoad<CR>'           , 'Load Session'}     ,
       s = {':Startify<CR>'        , 'Start Page'}     ,
       S = {':SSave<CR>'           , 'Save Session'}   ,
       },

    w = {
       name = '+windows' ,
       w = {'<C-W>w'     , 'other-window'}          ,
       d = {'<C-W>c'     , 'delete-window'}         ,
       ['-'] = {'<C-W>s'     , 'split-window-below'}    ,
       ['|'] = {'<C-W>v'     , 'split-window-right'}    ,
       ['2'] = {'<C-W>v'     , 'layout-double-columns'} ,
       h = {'<C-W>h'     , 'window-left'}           ,
       j = {'<C-W>j'     , 'window-below'}          ,
       l = {'<C-W>l'     , 'window-right'}          ,
       k = {'<C-W>k'     , 'window-up'}             ,
       H = {'<C-W>5<'    , 'expand-window-left'}    ,
       J = {':resize +5<CR>'  , 'expand-window-below'}   ,
       L = {'<C-W>5>'    , 'expand-window-right'}   ,
       K = {':resize -5<CR>'  , 'expand-window-up'}      ,
       ['='] = {'<C-W>='     , 'balance-window'}        ,
       s = {'<C-W>s'     , 'split-window-below'}    ,
       v = {'<C-W>v'     , 'split-window-below'}    ,
       ['?'] = {'Windows'    , 'fzf-window'}            ,
       },

    t = {
       name = '+toggles' ,
       b = {':GitBlameToggle<CR>'        , 'GitBlame'},
       t = {':TagbarToggle<CR>'        , 'Tagbar'},
       T = {':TagbarOpen j<CR>'        , 'Jump to Tagbar'},
       n = {':set number! relativenumber!<CR>'    , 'RelativeNumer'},
       },

    v = {
       name = '+vimwiki' ,
       w = {'<Plug>VimwikiIndex', 'VimwikiIndex'},
       t = {'<Plug>VimwikiTabIndex', 'VimwikiTabIndex'},
       s = {'<Plug>VimwikiUISelect', 'VimwikiUISelect'},
       i = {'<Plug>VimwikiDiaryIndex', 'VimwikiDiaryIndex'},
       I = {'<Plug>VimwikiDiaryGenerateLinks', 'VimwikiDiaryGenerateLinks'},
       W = {'<Plug>VimwikiMakeDiaryNote', 'VimwikiMakeDiaryNote'},
       T = {'<Plug>VimwikiTabMakeDiaryNote', 'VimwikiTabMakeDiaryNote'},
       y = {'<Plug>VimwikiMakeYesterdayDiaryNote', 'VimwikiMakeYesterdayDiaryNote'},
       m = {'<Plug>VimwikiMakeTomorrowDiaryNote', 'VimwikiMakeTomorrowDiaryNote'},
      },

    l = {
       name = '+lsp' ,
       a = {':Lspsaga code_action<CR>'                , 'code action'},
       A = {':Lspsaga range_code_action<CR>'          , 'selected action'},
       d = {':Telescope lsp_document_diagnostics<CR>' , 'document diagnostics'},
       D = {':Telescope lsp_workspace_diagnostics<CR>', 'workspace diagnostics'},
       f = {':lua vim.lsp.buf.formatting()<CR>'                      , 'format'},
       I = {':LspInfo<CR>'                            , 'lsp info'},
       v = {':LspVirtualTextToggle<CR>'               , 'lsp toggle virtual text'},
       l = {':Lspsaga lsp_finder<CR>'                 , 'lsp finder'},
       L = {':Lspsaga show_line_diagnostics<CR>'      , 'line_diagnostics'},
       p = {':Lspsaga preview_definition<CR>'         , 'preview definition'},
       q = {':Telescope quickfix<CR>'                 , 'quickfix'},
       r = {':Lspsaga rename<CR>'                     , 'rename'},
       T = {':lua vim.lsp.buf.type_definition()<CR>'                  , 'type defintion'},
       x = {':cclose<CR>'                             , 'close quickfix'},
       s = {':Telescope lsp_document_symbols<CR>'     , 'document symbols'},
       S = {':Telescope lsp_workspace_symbols<CR>'    , 'workspace symbols'},       
       g = {
       name = '+goto' ,
       d = {':LspDefinition<CR>'      , 'Goto Definition'},
       y = {':LspTypeDefinition<CR>' , 'Goto Type Definition'},
       i = {':LspImplementation<CR>'  , 'Goto Implementation'},
       r = {':LspReferences<CR>'      , 'Goto References'},
       }
	},

    T = {
       name = '+terminal' ,
       [';'] = {':FloatermNew --wintype=normal --height=6'       , 'terminal'},
       f = {':FloatermNew fzf<CR>'                               , 'fzf'},
       g = {':FloatermNew lazygit<CR>'                           , 'git'},
       d = {':FloatermNew lazydocker<CR>'                        , 'docker'},
       n = {':FloatermNew node<CR>'                              , 'node'},
       N = {':FloatermNew lf<CR>'                                , 'lf'},
       p = {':FloatermNew python<CR>'                            , 'python'},
       t = {':FloatermToggle<CR>'                                , 'toggle'},
       h = {':FloatermNew htop<CR>'                              , 'htop'},
       u = {':FloatermNew ncdu<CR>'                              , 'ncdu'},
       },

    p = {
       name = '+packages' ,
       c = {':PackerClean<CR>'      , 'PackerClean'},
       i = {':PackerInstall<CR>'      , 'PackerInstall'},
       u = {':PackerUpdate<CR>' , 'PackerUpdate'},
       s = {':PackerSync<CR>'  , 'PackerSync'},
       S = {':PackerStatus<CR>'      , 'PackerStatus'},
       },

    q = {
       name = '+vim' ,
       r = {':source $MYVIMRC<CR>'  , 'Goto Implementation'},
       q = {':qall<CR>'      , 'Quit Vim'},
       }
}

wk.register_keymap('leader', keymap)
