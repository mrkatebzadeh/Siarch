O_OPTIONS = {}

O_OPTIONS.setup = function()
  local status_ok, which_key = pcall(require, "which-key")
  if not status_ok then
    return
  end

  local opts = {
    mode = "n",
    prefix = "<leader>",
    buffer = nil,
    silent = true,
    noremap = true,
    nowait = true,
  }

  local mappings = {
    o = {
      name = "Options",
      w = { "<cmd>set wrap!<CR>", "Soft Wrap Text" },
      z = { "<cmd>ZenMode<CR>", "Zend Mode" },
      c = { "<cmd>ColorizerToggle<CR>", "Toggle Colorizer" },
      o = { "<cmd>Outline<CR>", "Toggle Outline" },
      i = { "<cmd>IBLToggle<cr>", "Indent Hints" },
      t = { "<cmd>TodoTelescope<cr>", "TODOs" },
      u = { "<cmd>Telescope undo<cr>", "Undo" },
    },
  }

  which_key.register(mappings, opts)

  opts = {
    mode = "v",
    prefix = "<leader>",
    buffer = nil,
    silent = true,
    noremap = true,
    nowait = true,
  }

  which_key.register(mappings, opts)
end

return O_OPTIONS
