G_GOTO = {}

G_GOTO.setup = function()
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
    G = {
      name = "Goto",
      d = { vim.lsp.buf.definition, "Definition" },
      r = { vim.lsp.buf.references, "References" },
      a = { vim.lsp.buf.code_action, "Action" },
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

return G_GOTO
