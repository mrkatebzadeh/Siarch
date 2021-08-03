local preview_settings = {}

local sumatrapdf_args = { "-reuse-instance", "%p", "-forward-search", "%f", "%l" }
local evince_args = { "-f", "%l", "%p", "\"code -g %f:%l\"" }
local okular_args = { "--unique", "file:%p#src:%l%f" }
local zathura_args = { "--synctex-forward", "%l:1:%f", "%p" }
local qpdfview_args = { "--unique", "%p#src:%f:%l:1" }
local skim_args = { "%l", "%p", "%f" }

preview_settings = zathura_args

require("lspconfig").texlab.setup {
  cmd = { DATA_PATH .. "/lspinstall/latex/texlab" },
  on_attach = require("lsp").common_on_attach,
  handlers = {
    ["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
      virtual_text = {spacing = 0, prefix = ""},
        signs = true,
        underline = true,
       update_in_insert = true,
    }),
  },
  filetypes = { "tex", "bib" },
  settings = {
    texlab = {
      aux_directory = ".",
      bibtex_formatter = "texlab",
      build = {
        executable = "latexmk",
        args = {'-pdf', '-interaction=nonstopmode', '-synctex=1', '%f'},
        on_save = false,
        forward_search_after = false,
      },
      chktex = {
        onEdit = false,
        onOpenAndSave = false,
      },
      diagnostics_delay = 300,
      formatter_line_length = 80,
      forwardSearch = {
        args = preview_settings,
        executable = "zathura",
      },
      latexFormatter = "latexindent",
      latexindent = {
         modifyLineBreaks = false,
      },
    },
  },
}
vim.g.vimtex_compiler_method = "latexmk"
vim.g.vimtex_view_method = "zathura"
vim.g.vimtex_fold_enabled = 0


-- Compile on initialization, cleanup on quit
vim.api.nvim_exec(
  [[
        augroup vimtex_event_1
            au!
            au User VimtexEventQuit     call vimtex#compiler#clean(0)
            au User VimtexEventInitPost call vimtex#compiler#compile()
        augroup END
    ]],
  false
)
