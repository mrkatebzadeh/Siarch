L_LSP = {}

L_LSP.setup = function()
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
    l = {
      name = "LSP",
      a = { "<cmd>lua vim.lsp.buf.code_action()<cr>", "Code Action" },
      b = {
        "<cmd>lua require('telescope.builtin').diagnostics({bufnr=0,layout_strategy='vertical',layout_config={width=0.9, height=0.95, preview_cutoff = 0}})<cr>",
        "Buffer Diagnostics",
      },
      c = {
        "<cmd>lua vim.lsp.codelens.refresh()<cr>",
        "Codelens Refresh",
      },
      d = {
        "<cmd>Telescope diagnostics bufnr=0<cr>",
        "Document Diagnostics",
      },
      D = {
        '<cmd>:lua vim.diagnostic.open_float({"line "})<cr>',
        "Show Line diagnostics",
      },
      e = {
        "<cmd>lua require('telescope').extensions.notify.notify({bufnr=0,layout_strategy='vertical',layout_config={width=0.9, height=0.95, preview_cutoff = 0},wrap_results=true,})<cr>",
        "Notofication History",
      },
      w = {
        "<cmd>Telescope diagnostics<cr>",
        "Workspace Diagnostics",
      },
      f = { "<cmd>lua vim.lsp.buf.format{async=true}<cr>", "Format" },
      i = { "<cmd>LspInfo<cr>", "Info" },
      I = { "<cmd>LspInstallInfo<cr>", "Installer Info" },
      j = {
        "<cmd>lua vim.lsp.diagnostic.goto_next()<CR>",
        "Next Diagnostic",
      },
      k = {
        "<cmd>lua vim.lsp.diagnostic.goto_prev()<cr>",
        "Prev Diagnostic",
      },
      l = { "<cmd>lua vim.lsp.codelens.run()<cr>", "CodeLens Action" },
      q = { "<cmd>lua vim.diagnostic.setloclist()<cr>", "Quickfix" },
      r = { "<cmd>lua vim.lsp.buf.rename()<cr>", "Rename" },
      s = { "<cmd>Telescope lsp_document_symbols<cr>", "Document Symbols" },
      S = {
        "<cmd>Telescope lsp_dynamic_workspace_symbols<cr>",
        "Workspace Symbols",
      },
      h = { vim.lsp.buf.hover, "Help" },
      H = { vim.lsp.buf.signature_help, "Signature Help" },
      t = { ":TroubleToggle<CR>", "Trouble" },
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

return L_LSP
