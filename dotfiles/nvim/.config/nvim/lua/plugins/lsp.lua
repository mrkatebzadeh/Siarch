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
			ensure_installed = { "clangd", "cmake", "rust_analyzer", "lua_ls", "texlab", "bashls" },
			auto_install = true,
		},
	},
	--[[ {
    "ray-x/lsp_signature.nvim",
    event = "VeryLazy",
    opts = {},
    config = function(_, opts)
      require("lsp_signature").setup(opts)
    end,
  }, ]]
	{
		"neovim/nvim-lspconfig",
		lazy = false,
		autostart = true,
		config = function()
			require("lspconfig.ui.windows").default_options.border = "rounded"
			local capabilities = require("cmp_nvim_lsp").default_capabilities()

			local lspconfig = require("lspconfig")

			lspconfig.verible.setup({
				cmd = { "verible-verilog-ls", "--rules_config_search" },
			})

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

      lspconfig.bashls.setup({
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
				"--enable-config", -- clangd 11+ supports reading from .clangd configuration file
				"--clang-tidy",
				"--offset-encoding=utf-16", --temporary fix for null-ls
				"--header-insertion-decorators",
				"--clang-tidy-checks=-*,llvm-*,clang-analyzer-*,modernize-*,-modernize-use-trailing-return-type,performance-*,bugprone-*",
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
	{
		"https://github.com/apple/pkl-neovim",
		lazy = true,
		event = "BufReadPre *.pkl",
		dependencies = {
			"nvim-treesitter/nvim-treesitter",
		},
		build = function()
			vim.cmd("TSInstall! pkl")
		end,
	},
	{
		"drmikehenry/vim-headerguard",
	},
}
