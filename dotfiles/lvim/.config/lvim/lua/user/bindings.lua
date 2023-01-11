local M = {}

M.config = function()
	lvim.leader = "space"
	lvim.builtin.which_key.setup.icons = {
		breadcrumb = lvim.icons.ui.DoubleChevronRight, -- symbol used in the command line area that shows your active key combo
		separator = "──", -- symbol used between a key and it's label
		group = "", -- symbol prepended to a group
	}
	lvim.keys.normal_mode["<C-s>"] = ":w<cr>"
	lvim.builtin.which_key.mappings["L"].name = " 🌙 LunarVim"
	lvim.builtin.which_key.mappings["b"].name = " 﩯 Buffers"
	lvim.builtin.which_key.mappings["d"].name = " 飯 Debug"
	lvim.builtin.which_key.mappings["g"].name = "   Git"
	lvim.builtin.which_key.mappings["l"].name = "   LSP"
	lvim.builtin.which_key.mappings["p"].name = "   Plugins"
	lvim.builtin.which_key.mappings["q"] = { "<cmd>lua require('lvim.utils.functions').smart_quit()<CR>", "   Quit"}
	lvim.builtin.which_key.mappings["s"].name = "   Search"
	lvim.builtin.which_key.mappings["b"].d = { ":bp<bar>sp<bar>bn<bar>bd<CR>", "Close Buffer" }
	lvim.builtin.which_key.mappings["c"] = {}
	lvim.builtin.which_key.mappings["e"] = {}
	lvim.builtin.which_key.mappings["T"] = {}
	lvim.builtin.which_key.mappings["f"] = {
		name = " 📁 Files",
		f = {
			function()
				require("lvim.core.telescope.custom-finders").find_project_files({ previewer = false })
			end,
			"Find File",
		},
		n = { "<cmd>NvimTreeToggle<CR>", "Explorer" },
		N = { ":NvimTreeOpen<CR>", "Toggle Focus" },
		F = { ":NvimTreeFindFile<CR>", "Tree Find" },
		w = { ":write<CR>", "Write File" },
		e = { ":e ", "e" },
		m = { ":Move", "Move" },
		d = { ":Delete<CR>", "Delete" },
		r = { ":Rename", "Rename" },
		c = { ":Chmod", "Chmod" },
		k = { ":Mkdir", "Mkdir" },
		W = { ":SudoWrite<CR>", "SudoWrite" },
		E = { ":SudoEdit", "SudoEdit" },
	}
	lvim.builtin.which_key.mappings["w"] = {
		name = " 🪟 Window",
		w = { "<C-W>w", "other-window" },
		d = { "<C-W>c", "delete-window" },
		["-"] = { "<C-W>s", "split-window-below" },
		["|"] = { "<C-W>v", "split-window-right" },
		["2"] = { "<C-W>v", "layout-double-columns" },
		h = { "<C-W>h", "window-left" },
		j = { "<C-W>j", "window-below" },
		l = { "<C-W>l", "window-right" },
		k = { "<C-W>k", "window-up" },
		H = { "<C-W>5<", "expand-window-left" },
		J = { ":resize +5<CR>", "expand-window-below" },
		L = { "<C-W>5>", "expand-window-right" },
		K = { ":resize -5<CR>", "expand-window-up" },
		["="] = { "<C-W>=", "balance-window" },
		s = { "<C-W>s", "split-window-below" },
		v = { "<C-W>v", "split-window-below" },
	}
	lvim.builtin.which_key.mappings["t"] = {
		name = " ✔️  Toggles",
		w = { ":set wrap!<CR>", "Soft Wrap Text" },
	}

	table.insert(lvim.builtin.nvimtree.setup.view.mappings.list, { key = { "<tab>" }, action = "preview", mode = "n" })
end

return M
