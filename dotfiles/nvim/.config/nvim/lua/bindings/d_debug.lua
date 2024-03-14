D_DEBUG = {}

D_DEBUG.setup = function()
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
    d = {
      name = "Debug",
      b = { ":DapToggleBreakpoint<CR>", "Toggle Breakpoint" },
      c = { ":DapContinue<CR>", "Continue" },
      o = { ":DapStepOver<CR>", "Step Over" },
      u = { ":DapStepOut<CR>", "Step Out" },
      i = { ":DapStepInto<CR>", "Step Into" },
      q = { ":DapTerminate<CR>", "Terminate" },
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

return D_DEBUG
