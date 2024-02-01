local status_ok, which_key = pcall(require, "which-key")
if not status_ok then
	return
end

local opts = {
	mode = "n", -- NORMAL mode
	prefix = "<leader>",
	buffer = nil, -- Global mappings. Specify a buffer number for buffer local mappings
	silent = true, -- use `silent` when creating keymaps
	noremap = true, -- use `noremap` when creating keymaps
	nowait = true, -- use `nowait` when creating keymaps
}

local mappings = {
	k = {
		name = "+C/C++",
		c = {
			name = "CMake",
			b = { "<cmd>CMakeBuild<Cr>", "CMakeBuild" },
			g = { "<cmd>CMakeGenerate<Cr>", "CMakeGenerate" },
			C = { "<cmd>CMakeClean<Cr>", "CMakeClean" },
			c = { "<cmd>CMakeClose<Cr>", "CMakeClose" },
			o = { "<cmd>CMakeOpen<Cr>", "CMakeOpen" },
		},
		s = { ":ClangdSwitchSourceHeader<cr>", "Header/Src" },
	},
}

which_key.register(mappings, opts)
