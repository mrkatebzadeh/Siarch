W_WINDOW = {}

W_WINDOW.setup = function()
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
    w = {
      name = "Window",
      o = { "<C-W>o", "Close Others" },
      w = { "<C-W>w", "Other" },
      d = { "<C-W>c", "Delete" },
      ["-"] = { "<C-W>s", "Split Below" },
      ["|"] = { "<C-W>v", "Split Right" },
      ["2"] = { "<C-W>v", "Layout Double Columns" },
      h = { "<C-W>h", "Left" },
      j = { "<C-W>j", "Below" },
      l = { "<C-W>l", "Right" },
      k = { "<C-W>k", "Up" },
      H = { "<C-W>5<", "Expand Left" },
      J = { "<cmd>resize +5<CR>", "Expand Below" },
      L = { "<C-W>5>", "Expand Right" },
      K = { "<cmd>resize -5<CR>", "Expand Up" },
      ["="] = { "<C-W>=", "Balance" },
      s = { "<C-W>s<C-W>w", "Split Focus Below" },
      v = { "<C-W>v<C-W>w", "Split Focus Right" },
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

return W_WINDOW
