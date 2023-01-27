-- Additional Plugins
local M = {}

M.config = function()
	lvim.plugins = {
		"windwp/nvim-spectre",
		{
			"lervag/vimtex",
			cmd = { "VimtexInfo", "VimtexCompile", "VimtexStop", "VimtexClean", "VimtexView" },
		},
		{
			"nekonako/xresources-nvim",
		},
		{
			"jackMort/ChatGPT.nvim",
			config = function()
				require("chatgpt").setup({
					-- optional configuration
				})
			end,
			requires = {
				"MunifTanjim/nui.nvim",
				"nvim-lua/plenary.nvim",
				"nvim-telescope/telescope.nvim",
			},
		},
		"simrat39/rust-tools.nvim",
		{
			"saecki/crates.nvim",
			tag = "v0.3.0",
			requires = { "nvim-lua/plenary.nvim" },
			config = function()
				require("crates").setup({
					null_ls = {
						enabled = true,
						name = "crates.nvim",
					},
					popup = {
						border = "rounded",
					},
				})
			end,
		},
		{
			"j-hui/fidget.nvim",
			config = function()
				require("fidget").setup()
			end,
		},
		{
			"akinsho/flutter-tools.nvim",
			requires = "nvim-lua/plenary.nvim",
			config = function()
				require("flutter-tools").setup({
					debugger = {
						enabled = true,
						run_via_dap = false,
						register_configurations = function(_)
							require("dap").configurations.dart = {}
							require("dap.ext.vscode").load_launchjs()
						end,
					},
					widget_guides = {
						enabled = true,
					},
					closing_tags = {
						prefix = "// ", -- character to use for close tag e.g. > Widget
						enabled = true, -- set to false to disable
					},
					dev_log = {
						enabled = true,
					},
					dev_tools = {
						autostart = false, -- autostart devtools server if not detected
						auto_open_browser = false, -- Automatically opens devtools in the browser
					},
					outline = {
						open_cmd = "30vnew", -- command to use to open the outline buffer
						auto_open = false, -- if true this will open the outline automatically when it is first populated
					},
					lsp = {
						on_attach = require("lvim.lsp").common_on_attach,
					},
				})
			end,
			ft = "dart",
		},
		{
			"kevinhwang91/nvim-bqf",
			event = { "BufRead", "BufNew" },
			config = function()
				require("bqf").setup({
					auto_enable = true,
					preview = {
						win_height = 12,
						win_vheight = 12,
						delay_syntax = 80,
						border_chars = { "┃", "┃", "━", "━", "┏", "┓", "┗", "┛", "█" },
					},
					func_map = {
						vsplit = "",
						ptogglemode = "z,",
						stoggleup = "",
					},
					filter = {
						fzf = {
							action_for = { ["ctrl-s"] = "split" },
							extra_opts = { "--bind", "ctrl-o:toggle-all", "--prompt", "> " },
						},
					},
				})
			end,
		},
		{
			"andymass/vim-matchup",
			event = "CursorMoved",
			config = function()
				vim.g.matchup_matchparen_offscreen = { method = "popup" }
			end,
		},
		{
			"rmagatti/goto-preview",
			config = function()
				require("goto-preview").setup({
					width = 120, -- Width of the floating window
					height = 25, -- Height of the floating window
					default_mappings = true, -- Bind default mappings
					debug = false, -- Print debug information
					opacity = nil, -- 0-100 opacity level of the floating window where 100 is fully transparent.
					post_open_hook = nil, -- A function taking two arguments, a buffer and a window to be ran as a hook.
				})
			end,
		},
		{
			"zbirenbaum/copilot.lua",
			event = { "VimEnter" },
			config = function()
				vim.defer_fn(function()
					require("copilot").setup({
						plugin_manager_path = get_runtime_dir() .. "/site/pack/packer",
						suggestion = {
							enabled = true,
							auto_trigger = true,
						},
					})
				end, 100)
			end,
		},

		{
			"zbirenbaum/copilot-cmp",
			after = { "copilot.lua", "nvim-cmp" },
			config = function()
				require("copilot_cmp").setup({
					formatters = {
						insert_text = require("copilot_cmp.format").remove_existing,
					},
				})

				lvim.builtin.cmp.formatting.source_names["copilot"] = "(Copilot)"
				table.insert(lvim.builtin.cmp.sources, 1, { name = "copilot" })
			end,
		},
		{ "jose-elias-alvarez/typescript.nvim" },
		"mxsdev/nvim-dap-vscode-js",
		{
			"folke/zen-mode.nvim",
			config = function()
				require("zen-mode").setup({
				})
			end,
		},
	}
end

return M
