return {
--[[
	"folke/noice.nvim",
	version = "v3.0.2",
	event = "VeryLazy",
	opts = {
		lsp = {
			override = {
				["vim.lsp.util.convert_input_to_markdown_lines"] = true,
				["vim.lsp.util.stylize_markdown"] = true,
				["cmp.entry.get_documentation"] = true,
			},
		},
		presets = {
			bottom_search = true,
			command_palette = true,
			long_message_to_split = true,
			inc_rename = false,
			lsp_doc_border = false,
		},
	},
	dependencies = {
		"MunifTanjim/nui.nvim",
		"rcarriga/nvim-notify",
	},
	config = function()
		local noice = require("noice")
		local noice_hl = vim.api.nvim_create_augroup("NoiceHighlights", {})
		local noice_cmd_types = {
			CmdLine = "Constant",
			Input = "Constant",
			Calculator = "Constant",
			Lua = "Constant",
			Filter = "Constant",
			Rename = "Constant",
			Substitute = "NoiceCmdlinePopupBorderSearch",
			Help = "Todo",
		}
		vim.api.nvim_clear_autocmds({ group = noice_hl })
		vim.api.nvim_create_autocmd("BufEnter", {
			group = noice_hl,
			desc = "redefinition of noice highlight groups",
			callback = function()
				for type, hl in pairs(noice_cmd_types) do
					vim.api.nvim_set_hl(0, "NoiceCmdlinePopupBorder" .. type, {})
					vim.api.nvim_set_hl(0, "NoiceCmdlinePopupBorder" .. type, { link = hl })
				end
				vim.api.nvim_set_hl(0, "NoiceConfirmBorder", {})
				vim.api.nvim_set_hl(0, "NoiceConfirmBorder", { link = "Constant" })
			end,
		})

		local cmdline_opts = {
			border = {
				style = "rounded",
				text = { top = "" },
			},
		}
		noice.setup({
			cmdline = {
				view = "cmdline_popup",
				format = {
					cmdline = { pattern = "^:", icon = "", opts = cmdline_opts },
					search_down = { view = "cmdline", kind = "Search", pattern = "^/", icon = "🔎 ", ft = "regex" },
					search_up = { view = "cmdline", kind = "Search", pattern = "^%?", icon = "🔎 ", ft = "regex" },
					input = { icon = "✏️ ", ft = "text", opts = cmdline_opts },
					calculator = { pattern = "^=", icon = "", lang = "vimnormal", opts = cmdline_opts },
					substitute = {
						pattern = "^:%%?s/",
						icon = "🔁",
						ft = "regex",
						opts = { border = { text = { top = " sub (old/new/) " } } },
					},
					filter = { pattern = "^:%s*!", icon = "$", ft = "sh", opts = cmdline_opts },
					filefilter = {
						kind = "Filter",
						pattern = "^:%s*%%%s*!",
						icon = "📄 $",
						ft = "sh",
						opts = cmdline_opts,
					},
					selectionfilter = {
						kind = "Filter",
						pattern = "^:%s*%'<,%'>%s*!",
						icon = " $",
						ft = "sh",
						opts = cmdline_opts,
					},
					lua = { pattern = "^:%s*lua%s+", icon = "", conceal = true, ft = "lua", opts = cmdline_opts },
					rename = {
						pattern = "^:%s*IncRename%s+",
						icon = "✏️ ",
						conceal = true,
						opts = {
							relative = "cursor",
							size = { min_width = 20 },
							position = { row = -3, col = 0 },
							buf_options = { filetype = "text" },
							border = { text = { top = " rename " } },
						},
					},
					help = { pattern = "^:%s*h%s+", icon = "💡", opts = cmdline_opts },
				},
			},
			messages = { view_search = false },
			lsp = {
				override = {
					["vim.lsp.util.convert_input_to_markdown_lines"] = true,
					["vim.lsp.util.stylize_markdown"] = true,
					["cmp.entry.get_documentation"] = true,
				},
				hover = { enabled = true },
				signature = { enabled = true },
				documentation = {
					opts = {
						win_options = {
							concealcursor = "n",
							conceallevel = 3,
							winhighlight = {
								Normal = "Normal",
								FloatBorder = "Todo",
							},
						},
					},
				},
			},
			views = {
				cmdline_popup = {
					position = {
						row = 5,
						col = "50%",
					},
					size = {
						width = 60,
						height = "auto",
					},
				},
				popupmenu = {
					relative = "editor",
					position = {
						row = 8,
						col = "50%",
					},
					size = {
						width = 60,
						height = 10,
					},
					border = {
						style = "rounded",
						padding = { 0, 1 },
					},
					win_options = {
						winhighlight = { Normal = "Normal", FloatBorder = "DiagnosticInfo" },
					},
				},
			},
			presets = {
				long_message_to_split = true,
				lsp_doc_border = true,
			},
			routes = {
				{ filter = { find = "E162" }, view = "mini" },
				{
					filter = {
						event = "msg_show",
						any = {
							{ find = "; after #%d+" },
							{ find = "; before #%d+" },
							{ find = "fewer lines" },
							{ find = "written" },
							{ find = "Conflict %[%d+" },
						},
					},
					view = "mini",
				},
				{ filter = { event = "msg_show", find = "search hit BOTTOM" }, skip = true },
				{ filter = { event = "msg_show", find = "search hit TOP" }, skip = true },
				{ filter = { event = "emsg", find = "E23" }, skip = true },
				{ filter = { event = "emsg", find = "E20" }, skip = true },
				{ filter = { find = "No signature help" }, skip = true },
				{ filter = { find = "E37" }, skip = true },
				{ filter = { find = "HEAD%-%d" }, skip = true },
				{ filter = { find = 'for "*"' }, skip = true },
			},
		})
	end, ]]
}
