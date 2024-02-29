R_REFACTOR = {}

R_REFACTOR.setup = function()
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
    r = {
      name = "Refactor",
      e = { ":Refactor extract ", "Extract" },
      f = { ":Refactor extract_to_file ", "Extract to File" },
      v = { ":Refactor extract_var ", "Extract Variable" },
      i = { ":Refactor inline_var<cr>", "Inline Variable" },
      I = { ":Refactor inline_func<cr>", "Inline Function" },
      b = { ":Refactor extract_block<cr>", "Extract Block" },
      B = { ":Refactor extract_block_to_file<cr>", "Extract Block to File" },
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

return R_REFACTOR
