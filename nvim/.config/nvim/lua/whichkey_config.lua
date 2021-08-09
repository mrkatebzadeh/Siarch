local wk = require "which-key"

local vmappings = {
      ["/"] = { ":CommentToggle<CR>", "Comment" },
    }
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
       e = {':e '               , 'e'},
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

    L = {
  		name = "+Latex",
	    b = { "<cmd>TexlabBuild<cr>", "Build with Texlab" },
        p = { "<cmd>TexlabForward<cr>", "Preview with Texlab" },
  		c = { "<cmd>VimtexCompile<cr>", "Toggle Compilation Mode" },
  		f = { "<cmd>call vimtex#fzf#run()<cr>", "Fzf Find" },
  		i = { "<cmd>VimtexInfo<cr>", "Project Information" },
  		s = { "<cmd>VimtexStop<cr>", "Stop Project Compilation" },
  		t = { "<cmd>VimtexTocToggle<cr>", "Toggle Table Of Content" },
  		v = { "<cmd>VimtexView<cr>", "View PDF" },
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
local setup = {
      plugins = {
        marks = true, -- shows a list of your marks on ' and `
        registers = true, -- shows your registers on " in NORMAL or <C-r> in INSERT mode
        -- the presets plugin, adds help for a bunch of default keybindings in Neovim
        -- No actual key bindings are created
        presets = {
          operators = false, -- adds help for operators like d, y, ...
          motions = false, -- adds help for motions
          text_objects = false, -- help for text objects triggered after entering an operator
          windows = true, -- default bindings on <c-w>
          nav = true, -- misc bindings to work with windows
          z = true, -- bindings for folds, spelling and others prefixed with z
          g = true, -- bindings for prefixed with g
        },
        spelling = { enabled = true, suggestions = 20 }, -- use which-key for spelling hints
      },
      icons = {
        breadcrumb = "»", -- symbol used in the command line area that shows your active key combo
        separator = "➜", -- symbol used between a key and it's label
        group = "+", -- symbol prepended to a group
      },
      window = {
        border = "single", -- none, single, double, shadow
        position = "bottom", -- bottom, top
        margin = { 1, 0, 1, 0 }, -- extra window margin [top, right, bottom, left]
        padding = { 2, 2, 2, 2 }, -- extra window padding [top, right, bottom, left]
      },
      layout = {
        height = { min = 4, max = 25 }, -- min and max height of the columns
        width = { min = 20, max = 50 }, -- min and max width of the columns
        spacing = 3, -- spacing between columns
      },
      hidden = { "<silent>", "<cmd>", "<Cmd>", "<CR>", "call", "lua", "^:", "^ " }, -- hide mapping boilerplate
      show_help = true, -- show help message on the command line when the popup is visible
    }
local opts = {
      mode = "n", -- NORMAL mode
      prefix = "<leader>",
      buffer = nil, -- Global mappings. Specify a buffer number for buffer local mappings
      silent = false, -- use `silent` when creating keymaps
      noremap = true, -- use `noremap` when creating keymaps
      nowait = true, -- use `nowait` when creating keymaps
    }
local vopts = {
      mode = "v", -- VISUAL mode
      prefix = "<leader>",
      buffer = nil, -- Global mappings. Specify a buffer number for buffer local mappings
      silent = true, -- use `silent` when creating keymaps
      noremap = true, -- use `noremap` when creating keymaps
      nowait = true, -- use `nowait` when creating keymaps
    }

local status_ok, which_key = pcall(require, "which-key")
if not status_ok then
          vim.notify('Error running config for ' .. 'which-key' .. ': ' .. vim.inspect(which_key), vim.log.levels.ERROR, {})
		  return
end
which_key.setup(setup)

wk.register(keymap, opts)
wk.register(vmappings, vopts)
