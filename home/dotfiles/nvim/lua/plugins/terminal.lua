return {
	"akinsho/toggleterm.nvim",
	config = function()
		local term = require("toggleterm")
		local Terminal = require("toggleterm.terminal").Terminal
		local htop = Terminal:new({ cmd = "htop", hidden = true, direction = "float", float_opts = { border = "rounded"} })

		function _HTOP_TOGGLE()
			htop:toggle()
		end

		local python = Terminal:new({ cmd = "python3", hidden = true, direction = "float", float_opts = {border = "rounded"} })

		function _PYTHON_TOGGLE()
			python:toggle()
		end

		term.setup({
			active = true,
			on_config_done = nil,
			-- size can be a number or function which is passed the current terminal
			size = 20,
			open_mapping = [[<c-\>]],
			hide_numbers = true, -- hide the number column in toggleterm buffers
			shade_filetypes = {},
			shade_terminals = true,
			shading_factor = 2, -- the degree by which to darken to terminal colour, default: 1 for dark backgrounds, 3 for light
			start_in_insert = true,
			insert_mappings = true, -- whether or not the open mapping applies in insert mode
			persist_size = false,
			-- direction = 'vertical' | 'horizontal' | 'window' | 'float',
			direction = "vertical",
			close_on_exit = true, -- close the terminal window when the process exits
			shell = nil, -- change the default shell
			-- This field is only relevant if direction is set to 'float'
			float_opts = {
				-- The border key is *almost* the same as 'nvim_win_open'
				-- see :h nvim_win_open for details on borders however
				-- the 'curved' border is a custom border type
				-- not natively supported but implemented in this plugin.
				-- border = 'single' | 'double' | 'shadow' | 'curved' | ... other options supported by win open
				border = "curved",
				-- width = <value>,
				-- height = <value>,
				winblend = 0,
				highlights = {
					border = "Normal",
					background = "Normal",
				},
			},
			-- Add executables on the config.lua
			-- { cmd, keymap, description, direction, size }
			-- lvim.builtin.terminal.execs = {...} to overwrite
			-- lvim.builtin.terminal.execs[#lvim.builtin.terminal.execs+1] = {"gdb", "tg", "GNU Debugger"}
			-- TODO: pls add mappings in which key and refactor this
			execs = {},
		})
	end,
}
