vim.api.nvim_create_autocmd("BufEnter", {
  group = vim.api.nvim_create_augroup("DisableNewLineAutoCommentString", {}),
  callback = function()
    vim.opt.formatoptions = vim.opt.formatoptions - { "c", "r", "o" }
  end,
})

-- LSP
local keymap = vim.keymap

local function preview_location_callback(_, result)
  if result == nil or vim.tbl_isempty(result) then
    return nil
  end
  local buf, _ = vim.lsp.util.preview_location(result[1])
  if buf then
    local cur_buf = vim.api.nvim_get_current_buf()
    vim.bo[buf].filetype = vim.bo[cur_buf].filetype
  end
end

local function peek_definition()
  local params = vim.lsp.util.make_position_params()
  return vim.lsp.buf_request(0, "textDocument/definition", params, preview_location_callback)
end

local function peek_type_definition()
  local params = vim.lsp.util.make_position_params()
  return vim.lsp.buf_request(0, "textDocument/typeDefinition", params, preview_location_callback)
end

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

    -- Attach plugins
    if client.server_capabilities.documentSymbolProvider then
      require("nvim-navic").attach(client, bufnr)
    end

    vim.cmd.setlocal("signcolumn=yes")
    vim.bo[bufnr].bufhidden = "hide"

    -- Enable completion triggered by <c-x><c-o>
    vim.bo[bufnr].omnifunc = "v:lua.vim.lsp.omnifunc"
    local function desc(description)
      return { noremap = true, silent = true, buffer = bufnr, desc = description }
    end
    keymap.set("n", "gD", vim.lsp.buf.declaration, desc("[lsp] go to declaration"))
    keymap.set("n", "gd", vim.lsp.buf.definition, desc("[lsp] go to definition"))
    keymap.set("n", "<leader>gt", vim.lsp.buf.type_definition, desc("[lsp] go to type definition"))
    keymap.set("n", "K", vim.lsp.buf.hover, desc("[lsp] hover"))
    keymap.set("n", "<leader>pd", peek_definition, desc("[lsp] peek definition"))
    keymap.set("n", "<leader>pt", peek_type_definition, desc("[lsp] peek type definition"))
    keymap.set("n", "gi", vim.lsp.buf.implementation, desc("[lsp] go to implementation"))
    keymap.set("n", "<C-k>", vim.lsp.buf.signature_help, desc("[lsp] signature help"))
    keymap.set("n", "<leader>lc", vim.lsp.codelens.run, desc("[lsp] run code lens"))
    keymap.set("n", "<leader>lC", vim.lsp.codelens.refresh, desc("[lsp] refresh code lenses"))
    keymap.set("n", "gr", vim.lsp.buf.references, desc("[lsp] find references"))
    if client.server_capabilities.inlayHintProvider then
      keymap.set("n", "<leader>oh", function()
        local current_setting = vim.lsp.inlay_hint.is_enabled(bufnr)
        vim.lsp.inlay_hint.enable(bufnr, not current_setting)
      end, desc("[lsp] toggle inlay hints"))
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
