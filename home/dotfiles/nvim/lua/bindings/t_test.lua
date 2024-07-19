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
      o = { "<cmd>lua require('neotest').output.open({ enter = true, auto_close = true })<cr>", "Output" },
      s = { "<cmd>lua require('neotest').summary.toggle()<cr>", "Summary" },
      l = { "<cmd>lua require('neotest').run.run_last()<cr>", "Run Last" },
      f = { "<cmd>lua require('neotest').run.run(vim.fn.expand('%'))<cr>", "Run File" },
      c = { "<cmd>lua require('neotest').run.run()<cr>", "Run Current Test" },
      S = { "<cmd>lua require('neotest').run.stop()<cr>", "Stop" },
      t = { "<cmd>lua require('neotest').output_panel.toggle()<cr>", "Toggle" },
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
