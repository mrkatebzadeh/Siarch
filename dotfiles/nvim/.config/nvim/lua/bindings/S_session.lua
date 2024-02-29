S_SESSION = {}

S_SESSION.setup = function()
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
    S = {
      name = "Session",
      s = { "<cmd>SessionManager save_current_session <cr>", "Save Session" },
      l = { "<cmd>SessionManager load_session<cr>", "Load Session" },
      d = { "<cmd>SessionManager delete_session<cr>", "Delete Session" },
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

return S_SESSION
