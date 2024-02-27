return {
	{
		"catppuccin/nvim",
		lazy = false,
		name = "catppuccin",
		priority = 1000,
		config = function()
			require("catppuccin").setup({
				flavour = "frappe",
				transparent_background = false,
				styles = { -- Handles the styles of general hi groups (see `:h highlight-args`):
					comments = { "italic" }, -- Change the style of comments
				},
				integrations = {
					cmp = true,
					gitsigns = true,
					nvimtree = true,
					treesitter = true,
					notify = true,
					mini = {
						enabled = true,
						indentscope_color = "",
					},
				},
			})

			vim.o.termguicolors = true
			vim.o.background = "dark"
			vim.cmd([[colorscheme catppuccin]])
		end,
	},
}
