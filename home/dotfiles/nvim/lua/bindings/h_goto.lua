H_GOTO = {}

H_GOTO.setup = function()
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
    h = {
      name = "Goto",
      d = { "<cmd>lua require('goto-preview').goto_preview_definition()<cr>", "Definition" },
      r = { "<cmd>lua require('goto-preview').goto_preview_references()<cr>", "References" },
      t = { "<cmd>lua require('goto-preview').goto_preview_type_definition()<cr>", "Type Definition" },
      i = { "<cmd>lua require('goto-preview').goto_preview_implementation()<cr>", "Implementation" },
      l = { "<cmd>lua require('goto-preview').goto_preview_declaration()<cr>", "Declaration" },
      c = { "<cmd>lua require('goto-preview').close_all_win()<cr>", "Close All Windows" },
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

return H_GOTO
