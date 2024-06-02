local status_ok, which_key = pcall(require, "which-key")
if not status_ok then
  return
end

local opts = {
  mode = "n",    -- NORMAL mode
  prefix = "<leader>",
  buffer = nil,  -- Global mappings. Specify a buffer number for buffer local mappings
  silent = true, -- use `silent` when creating keymaps
  noremap = true, -- use `noremap` when creating keymaps
  nowait = true, -- use `nowait` when creating keymaps
}

local mappings = {
  k = {
    name = "+Notebook",
    i = { "<CMD>MoltenInit<Cr>", "Init" },
    d = { "<CMD>MoltenDeinit<Cr>", "DeInit" },
    n = { "<CMD>MoltenNext<Cr>", "Next" },
    p = { "<CMD>MoltenPrev<Cr>", "Previous" },
    l = { "<CMD>MoltenEvaluateLine<CR>", "Evaluate Line" },
    v = { "<CMD>MoltenEvaluateVisual<Cr>", "Evaluate Visual" },
    c = { "<CMD>MoltenReevaluateCell<Cr>", "Evaluate Cell" },
    D = { "<CMD>MoltenDelete<Cr>", "Delete Cell" },
    o = { "<CMD>MoltenShowOutput<Cr>", "Show Output" },
    R = { "<CMD>MoltenRestart!<Cr>", "Restart" },
  },
}

which_key.register(mappings, opts)
