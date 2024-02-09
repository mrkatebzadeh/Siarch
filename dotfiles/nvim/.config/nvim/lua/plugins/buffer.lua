return {
	{
		"romgrk/barbar.nvim",
		dependencies = {
			"lewis6991/gitsigns.nvim",
			"nvim-tree/nvim-web-devicons",
		},
		init = function()
			vim.g.barbar_auto_setup = false
			require("barbar").setup({
				icons = {
					-- Configure the base icons on the bufferline.
					-- Valid options to display the buffer index and -number are `true`, 'superscript' and 'subscript'
					buffer_index = false,
					buffer_number = false,
					button = "x",
					-- Enables / disables diagnostic symbols
					diagnostics = {
						[vim.diagnostic.severity.ERROR] = { enabled = true, icon = "ﬀ" },
						[vim.diagnostic.severity.WARN] = { enabled = false },
						[vim.diagnostic.severity.INFO] = { enabled = false },
						[vim.diagnostic.severity.HINT] = { enabled = true },
					},
					gitsigns = {
						added = { enabled = true, icon = "+" },
						changed = { enabled = true, icon = "~" },
						deleted = { enabled = true, icon = "-" },
					},
					filetype = {
						-- Sets the icon's highlight group.
						-- If false, will use nvim-web-devicons colors
						custom_colors = false,
						-- Requires `nvim-web-devicons` if `true`
						enabled = true,
					},
					separator = { left = "▎", right = "" },
					-- If true, add an additional separator at the end of the buffer list
					separator_at_end = true,
					-- Configure the icons on the bufferline when modified or pinned.
					-- Supports all the base icon options.
					modified = { button = "●" },
					pinned = { button = "", filename = true },
					-- Use a preconfigured buffer appearance— can be 'default', 'powerline', or 'slanted'
					preset = "default",
					-- Configure the icons on the bufferline based on the visibility of a buffer.
					-- Supports all the base icon options, plus `modified` and `pinned`.
					alternate = { filetype = { enabled = false } },
					current = { buffer_index = true },
					inactive = { button = "×" },
					visible = { modified = { buffer_number = false } },
				},
			})
		end,
		opts = {},
		version = "^1.0.0",
	},
	{
		"hedyhli/outline.nvim",
		config = function()
			vim.keymap.set("n", "<leader>o", "<cmd>Outline<CR>", { desc = "Toggle Outline" })

			require("outline").setup({})
		end,
	},
	{
		"norcalli/nvim-colorizer.lua",
	},
	{ "onsails/lspkind.nvim" },
	{
		"lukas-reineke/indent-blankline.nvim",
		main = "ibl",
		config = function()
			local highlight = {
				"RainbowRed",
				"RainbowYellow",
				"RainbowBlue",
				"RainbowOrange",
				"RainbowGreen",
				"RainbowViolet",
				"RainbowCyan",
			}

			local hooks = require("ibl.hooks")
			-- create the highlight groups in the highlight setup hook, so they are reset
			-- every time the colorscheme changes
			hooks.register(hooks.type.HIGHLIGHT_SETUP, function()
				vim.api.nvim_set_hl(0, "RainbowRed", { fg = "#E06C75" })
				vim.api.nvim_set_hl(0, "RainbowYellow", { fg = "#E5C07B" })
				vim.api.nvim_set_hl(0, "RainbowBlue", { fg = "#61AFEF" })
				vim.api.nvim_set_hl(0, "RainbowOrange", { fg = "#D19A66" })
				vim.api.nvim_set_hl(0, "RainbowGreen", { fg = "#98C379" })
				vim.api.nvim_set_hl(0, "RainbowViolet", { fg = "#C678DD" })
				vim.api.nvim_set_hl(0, "RainbowCyan", { fg = "#56B6C2" })
			end)

			require("ibl").setup({ indent = { highlight = highlight } })
		end,
	},
	{
		"kylechui/nvim-surround",
		version = "*", -- Use for stability; omit to use `main` branch for the latest features
		event = "VeryLazy",
		config = function()
			require("nvim-surround").setup({
				-- Configuration here, or leave empty to use defaults
			})
		end,
	},
}
