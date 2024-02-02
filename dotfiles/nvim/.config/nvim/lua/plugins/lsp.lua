return {
  {
    "williamboman/mason.nvim",
    lazy = false,
    config = function()
      require("mason").setup()
    end,
  },
  {
    "williamboman/mason-lspconfig.nvim",
    lazy = false,
    opts = {
      ensure_installed = { "clangd", "cmake", "rust_analyzer", "lua_ls", "texlab" },
      auto_install = true,
    },
  },
  {
    "neovim/nvim-lspconfig",
    lazy = false,
    autostart = true,
    config = function()
      require("lspconfig.ui.windows").default_options.border = "rounded"
      local capabilities = require("cmp_nvim_lsp").default_capabilities()

      local lspconfig = require("lspconfig")
      lspconfig.texlab.setup({
        capabilities = capabilities,
      })
      lspconfig.tsserver.setup({
        capabilities = capabilities,
      })
      lspconfig.html.setup({
        capabilities = capabilities,
      })
      lspconfig.lua_ls.setup({
        capabilities = capabilities,
      })

      lspconfig.cmake.setup({
        capabilities = capabilities,
      })

      -- Clangd
      local clangd_flags = {
        "--all-scopes-completion",
        "--suggest-missing-includes",
        "--background-index",
        "--pch-storage=disk",
        "--cross-file-rename",
        "--log=info",
        "--completion-style=detailed",
        "--enable-config",      -- clangd 11+ supports reading from .clangd configuration file
        "--clang-tidy",
        "--offset-encoding=utf-16", --temporary fix for null-ls
        -- "--clang-tidy-checks=-*,llvm-*,clang-analyzer-*,modernize-*,-modernize-use-trailing-return-type",
        -- "--fallback-style=Google",
        -- "--header-insertion=never",
        -- "--query-driver=<list-of-white-listed-complers>"
      }

      local clangd_bin = "clangd"

      lspconfig.clangd.setup({
        capabilities = capabilities,
        virtual_text = true,
        cmd = { clangd_bin, unpack(clangd_flags) },
        init_options = {
          clangdFileStatus = true,
          clangdSemanticHighlighting = true,
        },
        filetypes = { "c", "h", "cpp", "cxx", "cc", "hpp" },
        settings = {
          ["clangd"] = {
            ["compilationDatabasePath"] = "build",
            ["fallbackFlags"] = { "-std=c++17" },
          },
        },
      })

      require("lspconfig").cmake.setup({})
    end,
  },
}
