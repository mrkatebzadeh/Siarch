T_TEST = {}

T_TEST.setup = function()
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
    t = {
      name = "Test",
      o = { "<cmd>Neotest output<cr>", "Output" },
      s = { "<cmd>Neotest summary<cr>", "Summary" },
      l = { "<cmd>Neotest run last<cr>", "Run Last" },
      f = { "<cmd>Neotest run file<cr>", "Run File" },
      c = { "<cmd>ConfigureGtest<cr>", "Configure" },
      S = { "<cmd>Neotest stop<cr>", "Stop" },
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

return T_TEST
