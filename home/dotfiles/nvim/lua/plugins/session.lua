return {
	{
    enabled = false,
		"Shatur/neovim-session-manager",
		config = function()
			local Path = require("plenary.path")
			local config = require("session_manager.config")
			local session_manager = require("session_manager")
			vim.api.nvim_create_autocmd({ "BufWritePre" }, {
				callback = function()
					for _, buf in ipairs(vim.api.nvim_list_bufs()) do
						-- Don't save while there's any 'nofile' buffer open.
						if vim.api.nvim_get_option_value("buftype", { buf = buf }) == "nofile" then
							return
						end
					end
					session_manager.save_current_session()
				end,
			})

			session_manager.setup({
				sessions_dir = Path:new(vim.fn.stdpath("data"), "sessions"),
				session_filename_to_dir = session_filename_to_dir,
				dir_to_session_filename = dir_to_session_filename,
				autoload_mode = config.AutoloadMode.CurrentDir,
				autosave_last_session = true,
				autosave_ignore_not_normal = true,
				autosave_ignore_dirs = {},
				autosave_ignore_filetypes = {
					"gitcommit",
					"gitrebase",
				},
				autosave_ignore_buftypes = {},
				autosave_only_in_session = false,
				max_path_length = 80,
			})
		end,
	},
}
