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
    name = "+ Rust",
    a = { "<cmd>:RustLsp codeAction<Cr>", "Actions" },
    r = { "<cmd>:RustLsp runnables<Cr>", "Runnables" },
    t = { "<cmd>:RustLsp testables<cr>", "Cargo Test" },
    m = { "<cmd>:RustLsp expandMacro<Cr>", "Expand Macro" },
    c = { "<cmd>:RustLsp openCargo<Cr>", "Open Cargo" },
    h = { "<cmd>:RustLsp externalDocs<Cr>", "Open Docs" },
    p = { "<cmd>:RustLsp parentModule<Cr>", "Parent Module" },
    d = { "<cmd>:RustLsp debuggables<Cr>", "Debuggables" },
    v = { "<cmd>:RustLsp crateGraph<Cr>", "View Crate Graph" },
    R = {
      "<cmd>lua require('rust-tools/workspace_refresh')._reload_workspace_from_cargo_toml()<Cr>",
      "Reload Workspace",
    },
    y = { "<cmd>lua require'crates'.open_repository()<cr>", "[crates] open repository" },
    P = { "<cmd>lua require'crates'.show_popup()<cr>", "[crates] show popup" },
    i = { "<cmd>lua require'crates'.show_crate_popup()<cr>", "[crates] show info" },
    f = { "<cmd>lua require'crates'.show_features_popup()<cr>", "[crates] show features" },
    D = { "<cmd>lua require'crates'.show_dependencies_popup()<cr>", "[crates] show dependencies" },
  },
}

which_key.register(mappings, opts)
