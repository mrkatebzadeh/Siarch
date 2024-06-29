COMMENT = {}

COMMENT.setup = function()
  local status_ok, which_key = pcall(require, "which-key")
  if not status_ok then
    return
  end

  local K = vim.keymap.set
  -- K("n", "K", vim.lsp.buf.hover, {})

  local call = require("Comment.api").call

  K("n", "<leader>/", call("toggle.linewise.current", "g@$"), { expr = true, desc = "Comment toggle current line" })

  K(
    "x",
    "<leader>/",
    '<ESC><CMD>lua require("Comment.api").locked("toggle.linewise")(vim.fn.visualmode())<CR>',
    { desc = "Comment toggle linewise (visual)" }
  )

  K(
    "x",
    "<leader>/",
    '<ESC><CMD>lua require("Comment.api").locked("toggle.blockwise")(vim.fn.visualmode())<CR>',
    { desc = "Comment toggle blockwise (visual)" }
  )

  local opts = {
    mode = "n",
    prefix = "<leader>",
    buffer = nil,
    silent = true,
    noremap = true,
    nowait = true,
  }

  local mappings = {
    ["/"] = {
      name = "Window",
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

return COMMENT
