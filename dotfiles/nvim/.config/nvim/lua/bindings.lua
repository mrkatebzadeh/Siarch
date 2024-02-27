local status_ok, which_key = pcall(require, "which-key")
if not status_ok then
	return
end

toggle_qf = function()
	local qf_exists = false
	for _, win in pairs(vim.fn.getwininfo()) do
		if win["quickfix"] == 1 then
			qf_exists = true
		end
	end
	if qf_exists == true then
		vim.cmd("cclose")
		return
	end
	if not vim.tbl_isempty(vim.fn.getqflist()) then
		vim.cmd("copen")
	end
end

local setup = {
	plugins = {
		marks = true, -- shows a list of your marks on ' and `
		registers = true, -- shows your registers on " in NORMAL or <C-r> in INSERT mode
		spelling = {
			enabled = true, -- enabling this will show WhichKey when pressing z= to select spelling suggestions
			suggestions = 20, -- how many suggestions should be shown in the list?
		},
		-- the presets plugin, adds help for a bunch of default keybindings in Neovim
		-- No actual key bindings are created
		presets = {
			operators = false, -- adds help for operators like d, y, ... and registers them for motion / text object completion
			motions = true, -- adds help for motions
			text_objects = true, -- help for text objects triggered after entering an operator
			windows = true, -- default bindings on <c-w>
			nav = true, -- misc bindings to work with windows
			z = true, -- bindings for folds, spelling and others prefixed with z
			g = true, -- bindings for prefixed with g
		},
	},
	-- add operators that will trigger motion and text object completion
	-- to enable all native operators, set the preset / operators plugin above
	-- operators = { gc = "Comments" },
	key_labels = {
		-- override the label used to display some keys. It doesn't effect WK in any other way.
		-- For example:
		-- ["<space>"] = "SPC",
		-- ["<cr>"] = "RET",
		-- ["<tab>"] = "TAB",
	},
	icons = {
		breadcrumb = "»", -- symbol used in the command line area that shows your active key combo
		separator = "➜", -- symbol used between a key and it's label
		group = "+", -- symbol prepended to a group
	},
	popup_mappings = {
		scroll_down = "<c-d>", -- binding to scroll down inside the popup
		scroll_up = "<c-u>", -- binding to scroll up inside the popup
	},
	window = {
		border = "rounded", -- none, single, double, shadow
		position = "bottom", -- bottom, top
		margin = { 1, 0, 1, 0 }, -- extra window margin [top, right, bottom, left]
		padding = { 2, 2, 2, 2 }, -- extra window padding [top, right, bottom, left]
		winblend = 0,
	},
	layout = {
		height = { min = 4, max = 25 }, -- min and max height of the columns
		width = { min = 20, max = 50 }, -- min and max width of the columns
		spacing = 3, -- spacing between columns
		align = "left", -- align columns left, center or right
	},
	ignore_missing = true, -- enable this to hide mappings for which you didn't specify a label
	hidden = { "<silent>", "<cmd>", "<Cmd>", "<CR>", "call", "lua", "^:", "^ " }, -- hide mapping boilerplate
	show_help = true, -- show help message on the command line when the popup is visible
	triggers = "auto", -- automatically setup triggers
	-- triggers = {"<leader>"} -- or specify a list manually
	triggers_blacklist = {
		-- list of mode / prefixes that should never be hooked by WhichKey
		-- this is mostly relevant for key maps that start with a native binding
		-- most people should not need to change this
		i = { "j", "k" },
		v = { "j", "k" },
	},
}

local opts = {
	mode = "n", -- NORMAL mode
	prefix = "<leader>",
	buffer = nil, -- Global mappings. Specify a buffer number for buffer local mappings
	silent = true, -- use `silent` when creating keymaps
	noremap = true, -- use `noremap` when creating keymaps
	nowait = true, -- use `nowait` when creating keymaps
}

local mappings = {
	a = { ":Alpha<CR>", "Alpha" },
	d = {
		name = "Debug",
		t = { ":DapToggleBreakpoint<CR>", "Toggle Breakpoin" },
		c = { ":DapContinue<CR>", "Continue" },
		x = { ":DapTerminate<CR>", "Terminate" },
		o = { ":DapStepOver<CR>", "StepOver" },
	},
	f = {
		name = "Files",
		n = { ":Neotree toggle filesystem<cr>", "NeoTree" },
		b = { ":Neotree buffers reveal float<cr>", "Buffers" },
		f = { "<cmd>Telescope find_files<cr>", "Find File" },
		g = { "<cmd>Telescope live_grep theme=ivy<cr>", "Find Text" },
		o = { "<cmd>Telescope oldfiles theme=ivy<cr>", "Old Files" },
		h = { "<cmd>Telescope help_tags theme=ivy<cr>", "Help Tags" },
		e = { ":e", "e" },
		m = { ":Move", "Move" },
		d = { ":Delete<CR>", "Delete" },
		r = { ":Rename", "Rename" },
		c = { ":Chmod", "Chmod" },
		k = { ":Mkdir", "Mkdir" },
		w = { "<cmd>w!<CR>", "Save" },
		W = { ":SudoWrite<CR>", "SudoWrite" },
		E = { ":SudoEdit", "SudoEdit" },
	},
	b = {
		name = "Buffers",
		b = {
			"<cmd>lua require('telescope.builtin').buffers(require('telescope.themes').get_dropdown{previewer = false})<cr>",
			"List Buffers",
		},
		d = { ":bp<bar>sp<bar>bn<bar>bd<CR>", "Close Buffer" },
		o = { '<cmd>%bdelete|edit #|normal`"<CR>', "Delete Others" },
		t = { "<cmd>lua toggle_qf()<CR>", "Toggle Quickfix" },
	},
	q = { "<cmd>qa!<CR>", "Quit" },
	P = { "<cmd>lua require('telescope').extensions.projects.projects()<cr>", "Projects" },
	-- git
	g = {
		name = "Git",
		s = { ":LazyGit<CR>", "Lazygit" },
		g = { ":LazyGit<CR>", "Lazygit" },
		j = { "<cmd>lua require 'gitsigns'.next_hunk()<cr>", "Next Hunk" },
		k = { "<cmd>lua require 'gitsigns'.prev_hunk()<cr>", "Prev Hunk" },
		l = { "<cmd>lua require 'gitsigns'.blame_line()<cr>", "Blame" },
		p = { "<cmd>lua require 'gitsigns'.preview_hunk()<cr>", "Preview Hunk" },
		r = { "<cmd>lua require 'gitsigns'.reset_hunk()<cr>", "Reset Hunk" },
		R = { "<cmd>lua require 'gitsigns'.reset_buffer()<cr>", "Reset Buffer" },
		S = { "<cmd>lua require 'gitsigns'.stage_hunk()<cr>", "Stage Hunk" },
		u = {
			"<cmd>lua require 'gitsigns'.undo_stage_hunk()<cr>",
			"Undo Stage Hunk",
		},
		o = { "<cmd>Telescope git_status<cr>", "Open Changed File" },
		b = { "<cmd>Telescope git_branches<cr>", "Checkout Branch" },
		c = { "<cmd>Telescope git_commits<cr>", "Checkout Commit" },
		d = {
			"<cmd>Gitsigns diffthis HEAD<cr>",
			"Diff",
		},
	},
	G = {
		name = "",
		d = { vim.lsp.buf.definition, "Definition" },
		r = { vim.lsp.buf.references, "References" },
		a = { vim.lsp.buf.code_action, "Action" },
	},
	-- lsp
	l = {
		name = "LSP",
		a = { "<cmd>lua vim.lsp.buf.code_action()<cr>", "Code Action" },
		b = {
			"<cmd>lua require('telescope.builtin').diagnostics({bufnr=0,layout_strategy='vertical',layout_config={width=0.9, height=0.95, preview_cutoff = 0}})<cr>",
			"Buffer Diagnostics",
		},
		d = {
			"<cmd>Telescope diagnostics bufnr=0<cr>",
			"Document Diagnostics",
		},
		D = {
			'<cmd>:lua vim.diagnostic.open_float({"line "})<cr>',
			"Show Line diagnostics",
		},
		e = {
			"<cmd>lua require('telescope').extensions.notify.notify({bufnr=0,layout_strategy='vertical',layout_config={width=0.9, height=0.95, preview_cutoff = 0},wrap_results=true,})<cr>",
			"Notofication History",
		},
		w = {
			"<cmd>Telescope diagnostics<cr>",
			"Workspace Diagnostics",
		},
		f = { "<cmd>lua vim.lsp.buf.format{async=true}<cr>", "Format" },
		i = { "<cmd>LspInfo<cr>", "Info" },
		I = { "<cmd>LspInstallInfo<cr>", "Installer Info" },
		j = {
			"<cmd>lua vim.lsp.diagnostic.goto_next()<CR>",
			"Next Diagnostic",
		},
		k = {
			"<cmd>lua vim.lsp.diagnostic.goto_prev()<cr>",
			"Prev Diagnostic",
		},
		l = { "<cmd>lua vim.lsp.codelens.run()<cr>", "CodeLens Action" },
		q = { "<cmd>lua vim.diagnostic.setloclist()<cr>", "Quickfix" },
		r = { "<cmd>lua vim.lsp.buf.rename()<cr>", "Rename" },
		s = { "<cmd>Telescope lsp_document_symbols<cr>", "Document Symbols" },
		S = {
			"<cmd>Telescope lsp_dynamic_workspace_symbols<cr>",
			"Workspace Symbols",
		},
		h = { vim.lsp.buf.hover, "Help" },
		t = { ":TroubleToggle<CR>", "Trouble" },
	},
	-- search
	s = {
		name = "Search",
		c = { "<cmd>Telescope colorscheme<cr>", "Colorscheme" },
		h = { "<cmd>Telescope help_tags<cr>", "Find Help" },
		M = { "<cmd>Telescope man_pages<cr>", "Man Pages" },
		r = { "<cmd>lua require('spectre').open()<cr>", "Replace" },
		R = { "<cmd>Telescope registers<cr>", "Registers" },
		k = { "<cmd>Telescope keymaps<cr>", "Keymaps" },
		C = { "<cmd>Telescope commands<cr>", "Commands" },
		w = { "<cmd>lua require('spectre').open_visual({select_word=true})<cr>", "Replace Word" },
		f = { "<cmd>lua require('spectre').open_file_search()<cr>", "Replace Buffer" },
	},
	-- session
	S = {
		name = "Session",
		s = { "<cmd>:SessionManager save_current_session <cr>", "Save Session" },
		l = { "<cmd>:SessionManager load_session<cr>", "Load Session" },
		d = { "<cmd>:SessionManager delete_session<cr>", "Delete Session" },
	},
	-- Test
	t = {
		name = "Test",
		o = { "<cmd>Neotest output<cr>", "Output" },
		s = { "<cmd>Neotest summary<cr>", "Summary" },
		l = { "<cmd>Neotest run last<cr>", "Run Last" },
		f = { "<cmd>Neotest run file<cr>", "Run File" },
		c = { "<cmd>ConfigureGtest<cr>", "Configure" },
		S = { "<cmd>Neotest stop<cr>", "Stop" },
	},
	-- terminal
	T = {
		name = "Terminal",
		t = { "<cmd>lua _HTOP_TOGGLE()<cr>", "Htop" },
		p = { "<cmd>lua _PYTHON_TOGGLE()<cr>", "Python" },
		f = { "<cmd>ToggleTerm direction=float<cr>", "Float" },
		h = { "<cmd>ToggleTerm size=10 direction=horizontal<cr>", "Horizontal" },
		v = { "<cmd>ToggleTerm size=80 direction=vertical<cr>", "Vertical" },
	},
	-- project
	p = {
		name = "Project",
		p = { "<cmd>:Telescope projects<cr>", "Find Project Files" },
		-- b = { "<cmd>:lua require('project_nvim').browse_project_files()<cr>", "Browse Project Files" },
		-- d = { "<cmd>:lua require('project_nvim').delete_project()<cr>", "Delete Project" },
		-- s = { "<cmd>:lua require('project_nvim').search_in_project_files()<cr>", "Search in Project Files" },
		-- p = { "<cmd>:lua require('project_nvim').recent_projects()<cr>", "Recent Projects" },
		-- r = { "<cmd>:lua require('project_nvim').recent_project_files()<cr>", "Recent Project Files" },
		-- w = { "<cmd>:lua require('project_nvim').change_working_directory()<cr>", "Change Working Directory" },
	},
	-- neorg
	n = {
		name = "Neorg",
		r = { "<cmd>Neorg workspace research<cr>", "Research" },
		h = { "<cmd>Neorg workspace home<cr>", "Home" },
		t = { "<cmd>Neorg toggle-concealer<cr>", "Toggle" },
		g = { "<cmd>Neorg keybind all core.looking-glass.magnify-code-block<cr>", "Glass" },
		q = { "<cmd>Neorg return<cr>", "Quit/Return" },
	},
	-- customize
	o = {
		name = "Options",
		w = { ":set wrap!<CR>", "Soft Wrap Text" },
		z = { ":ZenMode<CR>", "Zend Mode" },
		c = { "<cmd>ColorizerToggle<CR>", "Toggle Colorizer" },
		o = { "<cmd>Outline<CR>", "Toggle Outline" },
		i = { "<cmd>IBLToggle<cr>", "Indent Hints" },
		t = { "<cmd>TodoTelescope<cr>", "TODOs" },
		u = { "<cmd>Telescope undo<cr>", "Undo" },
	},
	-- ChatGPT
	C = {
		name = "ChatGPT",
		c = { "<cmd>ChatGPT<CR>", "ChatGPT" },
		e = { "<cmd>ChatGPTEditWithInstruction<CR>", "Edit with Instruction", mode = { "n", "v" } },
		g = { "<cmd>ChatGPTRun grammar_correction<CR>", "Grammar Correction", mode = { "n", "v" } },
		t = { "<cmd>ChatGPTRun translate<CR>", "Translate", mode = { "n", "v" } },
		k = { "<cmd>ChatGPTRun keywords<CR>", "Keywords", mode = { "n", "v" } },
		d = { "<cmd>ChatGPTRun docstring<CR>", "Docstring", mode = { "n", "v" } },
		a = { "<cmd>ChatGPTRun add_tests<CR>", "Add Tests", mode = { "n", "v" } },
		o = { "<cmd>ChatGPTRun optimize_code<CR>", "Optimize Code", mode = { "n", "v" } },
		s = { "<cmd>ChatGPTRun summarize<CR>", "Summarize", mode = { "n", "v" } },
		f = { "<cmd>ChatGPTRun fix_bugs<CR>", "Fix Bugs", mode = { "n", "v" } },
		x = { "<cmd>ChatGPTRun explain_code<CR>", "Explain Code", mode = { "n", "v" } },
		r = { "<cmd>ChatGPTRun roxygen_edit<CR>", "Roxygen Edit", mode = { "n", "v" } },
		l = { "<cmd>ChatGPTRun code_readability_analysis<CR>", "Code Readability Analysis", mode = { "n", "v" } },
	},
	-- Refactor
	r = {
		name = "Refactor",
		e = { ":Refactor extract ", "Extract" },
		f = { ":Refactor extract_to_file ", "Extract to File" },
		v = { ":Refactor extract_var ", "Extract Variable" },
		i = { ":Refactor inline_var<cr>", "Inline Variable" },
		I = { ":Refactor inline_func<cr>", "Inline Function" },
		b = { ":Refactor extract_block<cr>", "Extract Block" },
		B = { ":Refactor extract_block_to_file<cr>", "Extract Block to File" },
	},
	-- window
	w = {
		name = "Window",
		o = { "<C-W>o", "Close Others" },
		w = { "<C-W>w", "Other" },
		d = { "<C-W>c", "Delete" },
		["-"] = { "<C-W>s", "Split Below" },
		["|"] = { "<C-W>v", "Split Right" },
		["2"] = { "<C-W>v", "Layout Double Columns" },
		h = { "<C-W>h", "Left" },
		j = { "<C-W>j", "Below" },
		l = { "<C-W>l", "Right" },
		k = { "<C-W>k", "Up" },
		H = { "<C-W>5<", "Expand Left" },
		J = { ":resize +5<CR>", "Expand Below" },
		L = { "<C-W>5>", "Expand Right" },
		K = { ":resize -5<CR>", "Expand Up" },
		["="] = { "<C-W>=", "Balance" },
		s = { "<C-W>s<C-W>w", "Split Focus Below" },
		v = { "<C-W>v<C-W>w", "Split Focus Right" },
	},
}

which_key.setup(setup)
which_key.register(mappings, opts)

opts = {
	mode = "v", -- NORMAL mode
	prefix = "<leader>",
	buffer = nil, -- Global mappings. Specify a buffer number for buffer local mappings
	silent = true, -- use `silent` when creating keymaps
	noremap = true, -- use `noremap` when creating keymaps
	nowait = true, -- use `nowait` when creating keymaps
}

mappings = {
	-- Refactor
	r = {
		name = "Refactor",
		e = { ":Refactor extract ", "Extract" },
		f = { ":Refactor extract_to_file ", "Extract to File" },
		v = { ":Refactor extract_var ", "Extract Variable" },
		i = { ":Refactor inline_var<cr>", "Inline Variable" },
		I = { ":Refactor inline_func<cr>", "Inline Function" },
		b = { ":Refactor extract_block<cr>", "Extract Block" },
		B = { ":Refactor extract_block_to_file<cr>", "Extract Block to File" },
	},
}

which_key.setup(setup)
which_key.register(mappings, opts)

local K = vim.keymap.set
K("n", "K", vim.lsp.buf.hover, {})

local call = require("Comment.api").call

K("n", "<leader>/", call("toggle.linewise.current", "g@$"), { expr = true, desc = "Comment toggle current line" })

K(
	"x",
	"<leader>/",
	'<ESC><CMD>lua require("Comment.api").locked("toggle.linewise")(vim.fn.visualmode())<CR>',
	{ desc = "Comment toggle linewise (visual)" }
)

K(
	"x",
	"<leader>/",
	'<ESC><CMD>lua require("Comment.api").locked("toggle.blockwise")(vim.fn.visualmode())<CR>',
	{ desc = "Comment toggle blockwise (visual)" }
)
