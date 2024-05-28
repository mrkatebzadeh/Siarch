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
		name = "Rust",
		a = { "<cmd>:RustLsp codeAction<Cr>", "Actions" },
		r = { "<cmd>:RustLsp runnables<Cr>", "Runnables" },
		t = { "<cmd>:RustLsp testables<cr>", "Cargo Test" },
		m = { "<cmd>:RustLsp expandMacro<Cr>", "Expand Macro" },
		C = { "<cmd>:RustLsp openCargo<Cr>", "Open Cargo" },
		h = { "<cmd>:RustLsp externalDocs<Cr>", "Open Docs" },
		p = { "<cmd>:RustLsp parentModule<Cr>", "Parent Module" },
		d = { "<cmd>:RustLsp debuggables<Cr>", "Debuggables" },
		v = { "<cmd>:RustLsp crateGraph<Cr>", "View Crate Graph" },
		R = { "<cmd>:RustLsp reloadWorkspace<Cr>", "Reload Workspace" },
		c = {
			name = "Crate",
			y = { "<cmd>lua require'crates'.open_repository()<cr>", "Open Repository" },
			p = { "<cmd>lua require'crates'.show_popup()<cr>", "Show Popup" },
			i = { "<cmd>lua require'crates'.show_crate_popup()<cr>", "Show Info" },
			f = { "<cmd>lua require'crates'.show_features_popup()<cr>", "Show Features" },
			d = { "<cmd>lua require'crates'.show_dependencies_popup()<cr>", "Show Dependencies" },
		},
	},
}

which_key.register(mappings, opts)
