return {
	{
		enabled = false,
		"m4xshen/hardtime.nvim",
		dependencies = { "MunifTanjim/nui.nvim", "nvim-lua/plenary.nvim" },
		opts = {},
		config = function()
			require("hardtime").setup({
				disabled_filetypes = { "qf", "NeoTree", "netrw", "NvimTree", "lazy", "mason", "oil" },
				max_count = 10,
			})
		end,
	},
}
