N_NOTE = {}

N_NOTE.setup = function()
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
    n = {
      name = "Note",
      r = { "<cmd>Neorg workspace research<cr>", "Research" },
      h = { "<cmd>Neorg workspace home<cr>", "Home" },
      c = { "<cmd>Neorg toggle-concealer<cr>", "Toggle" },
      g = { "<cmd>Neorg keybind all core.looking-glass.magnify-code-block<cr>", "Glass" },
      q = { "<cmd>Neorg return<cr>", "Quit/Return" },
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

return N_NOTE
