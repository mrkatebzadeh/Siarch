local status_ok, which_key = pcall(require, "which-key")
if not status_ok then
  return
end

local clangd_flags = {
  "--all-scopes-completion",
  "--suggest-missing-includes",
  "--background-index",
  "--pch-storage=disk",
  "--cross-file-rename",
  "--log=info",
  "--completion-style=detailed",
  "--enable-config",          -- clangd 11+ supports reading from .clangd configuration file
  "--clang-tidy",
  "--offset-encoding=utf-16", --temporary fix for null-ls
  -- "--clang-tidy-checks=-*,llvm-*,clang-analyzer-*,modernize-*,-modernize-use-trailing-return-type",
  -- "--fallback-style=Google",
  -- "--header-insertion=never",
  -- "--query-driver=<list-of-white-listed-complers>"
}

local clangd_bin = "clangd"

local c_opts = {
  cmd = { clangd_bin, unpack(clangd_flags) },
}
require("lvim.lsp.manager").setup("clangd", c_opts)

local opts = {
  mode = "n",     -- NORMAL mode
  prefix = "<leader>",
  buffer = nil,   -- Global mappings. Specify a buffer number for buffer local mappings
  silent = true,  -- use `silent` when creating keymaps
  noremap = true, -- use `noremap` when creating keymaps
  nowait = true,  -- use `nowait` when creating keymaps
}

local mappings = {
  k = {
    name = "+C/C++",
    b = { "<cmd>CMakeBuild<Cr>", "CMakeBuild" },
    g = { "<cmd>CMakeGenerate<Cr>", "CMakeGenerate" },
    C = { "<cmd>CMakeClean<Cr>", "CMakeClean" },
    c = { "<cmd>CMakeClose<Cr>", "CMakeClose" },
    o = { "<cmd>CMakeOpen<Cr>", "CMakeOpen" },
  },
}

which_key.register(mappings, opts)
