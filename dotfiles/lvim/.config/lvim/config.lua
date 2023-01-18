--[[
lvim is the global options object
]]

-- general
lvim.log.level = "warn"
lvim.format_on_save.enabled = false
lvim.colorscheme = "xresources"
vim.g.vimtex_view_method = "zathura"
vim.wo.relativenumber = true
-- to disable icons and use a minimalist setup, uncomment the following
-- lvim.use_icons = false
lvim.lsp.override = { "dart" }
lvim.builtin.alpha.active = true
lvim.builtin.alpha.mode = "dashboard"
lvim.builtin.terminal.active = true
lvim.builtin.nvimtree.setup.view.side = "left"
lvim.builtin.nvimtree.setup.renderer.icons.show.git = false
lvim.lsp.diagnostics.virtual_text = false

-- if you don't want all the parsers change this to a table of the ones you want
lvim.builtin.treesitter.ensure_installed = {
  "bash",
  "c",
  "javascript",
  "json",
  "lua",
  "python",
  "typescript",
  "tsx",
  "css",
  "rust",
  "yaml",
  "latex",
  "dart",
}

lvim.builtin.treesitter.ignore_install = { "haskell" }
lvim.builtin.treesitter.highlight.enable = true

-- -- make sure server will always be installed even if the server is in skipped_servers list
lvim.lsp.installer.setup.ensure_installed = {
  "sumneko_lua",
}

-- -- set additional linters
local linters = require("lvim.lsp.null-ls.linters")
linters.setup({
  { command = "shellcheck", filetypes = { "sh" } },
})

-- vim.list_extend(lvim.lsp.automatic_configuration.skipped_servers, { "rust_analyzer" })
local formatters = require("lvim.lsp.null-ls.formatters")
formatters.setup({
  { command = "stylua", filetypes = { "lua" } },
  { command = "shfmt", filetypes = { "sh" } },
  { command = "dart_format", filetypes = { "dart" } },
})

require("user.plugins").config()
require("user.rust").config()
require("user.js-ts").config()
require("user.dap").config()
require("user.spectre").config()
require("user.nvimtree").config()
require("user.bindings").config()
