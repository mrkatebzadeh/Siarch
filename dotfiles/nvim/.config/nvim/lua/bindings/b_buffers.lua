B_BUFFERS = {}

B_BUFFERS.setup = function()
  local status_ok, which_key = pcall(require, "which-key")
  if not status_ok then
    return
  end

  function toggle_qf ()
    local qf_exists = false
    for _, win in pairs(vim.fn.getwininfo()) do
      if win["quickfix"] == 1 then
        qf_exists = true
      end
    end
    if qf_exists == true then
      vim.cmd("cclose")
      return
    end
    if not vim.tbl_isempty(vim.fn.getqflist()) then
      vim.cmd("copen")
    end
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
    b = {
      name = "Buffers",
      b = {
        "<cmd>lua require('telescope.builtin').buffers(require('telescope.themes').get_dropdown{previewer = false})<cr>",
        "List Buffers",
      },
      d = { ":bp<bar>sp<bar>bn<bar>bd<CR>", "Close Buffer" },
      o = { '<cmd>%bdelete|edit #|normal`"<CR>', "Delete Others" },
      t = { "<cmd>lua toggle_qf()<CR>", "Toggle Quickfix" },
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

return B_BUFFERS
