--- Don't create a comment string when hitting <Enter> on a comment line
vim.api.nvim_create_autocmd("BufEnter", {
  group = vim.api.nvim_create_augroup("DisableNewLineAutoCommentString", {}),
  callback = function()
    vim.opt.formatoptions = vim.opt.formatoptions - { "c", "r", "o" }
  end,
})

vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("UserLspConfig", {}),
  callback = function(ev)
    local bufnr = ev.buf
    local client = vim.lsp.get_client_by_id(ev.data.client_id)

    vim.cmd.setlocal("signcolumn=yes")
    vim.bo[bufnr].bufhidden = "hide"

    -- Enable completion triggered by <c-x><c-o>
    vim.bo[bufnr].omnifunc = "v:lua.vim.lsp.omnifunc"
    local function desc(description)
      return { noremap = true, silent = true, buffer = bufnr, desc = description }
    end

    if client.server_capabilities.inlayHintProvider then
      vim.lsp.inlay_hint.enable(bufnr)
      local wk = require("which-key")
      local opts = {
        mode = "n",
        prefix = "<leader>",
        buffer = nil,
        silent = true,
        noremap = true,
        nowait = true,
      }
      wk.register({
        o = {
          h = {
            function()
              local current_setting = vim.lsp.inlay_hint.is_enabled(bufnr)
              vim.lsp.inlay_hint.enable(bufnr, not current_setting)
            end,
            "Toggle Inlay Hints",
          },
        },
      }, opts)
    end

    -- Auto-refresh code lenses
    if not client then
      return
    end
    local function buf_refresh_codeLens()
      vim.schedule(function()
        if client.server_capabilities.codeLensProvider then
          vim.lsp.codelens.refresh()
          return
        end
      end)
    end
    local group = vim.api.nvim_create_augroup(string.format("lsp-%s-%s", bufnr, client.id), {})
    if client.server_capabilities.codeLensProvider then
      vim.api.nvim_create_autocmd({ "InsertLeave", "BufWritePost", "TextChanged" }, {
        group = group,
        callback = buf_refresh_codeLens,
        buffer = bufnr,
      })
      buf_refresh_codeLens()
    end
  end,
})

vim.diagnostic.config({
  virtual_text = false,
  signs = true,
  float = {
    border = "single",
    format = function(diagnostic)
      return string.format(
        "%s (%s) [%s]",
        diagnostic.message,
        diagnostic.source,
        diagnostic.code or diagnostic.user_data.lsp.code
      )
    end,
  },
})

local icons = require("icons")

vim.fn.sign_define("DiagnosticSignError", { text = icons.DiagnosticError, texthl = "DiagnosticSignError" })
vim.fn.sign_define("DiagnosticSignWarn", { text = icons.DiagnosticWarn, texthl = "DiagnosticSignWarn" })
vim.fn.sign_define("DiagnosticSignInfo", { text = icons.DiagnosticInfo, texthl = "DiagnosticSignInfo" })
vim.fn.sign_define("DiagnosticSignHint", { text = icons.DiagnosticHint, texthl = "DiagnosticSignHint" })
