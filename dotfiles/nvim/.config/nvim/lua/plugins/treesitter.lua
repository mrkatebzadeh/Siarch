return {
	{
		"nvim-treesitter/nvim-treesitter",
    version = "v0.9.2",
		build = ":TSUpdate",
		config = function()
			local config = require("nvim-treesitter.configs")
			config.setup({
				ensure_installed = { "c", "lua", "rust", "bash", "vim", "html", "bash" },
				auto_install = true,
				highlight = { enable = true },
				indent = { enable = true },
				autotag = {
					enable = true,
				},
			})
		end,
	},
  {
    "windwp/nvim-ts-autotag"
  }
}
