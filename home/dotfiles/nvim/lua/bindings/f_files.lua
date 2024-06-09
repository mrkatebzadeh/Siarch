F_FILES = {}

F_FILES.setup = function()
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
    f = {
      name = "Files",
      n = { "<cmd>Telescope file_browser path=%:p:h select_buffer=true<cr>", "File Browser" },
      N = { "<cmd>Telescope file_browser <cr>", "File Browser" },
      e = { "<cmd>Neotree toggle filesystem<cr>", "NeoTree" },
      b = { "<cmd>Neotree buffers reveal float<cr>", "Buffers" },
      f = { "<cmd>Telescope find_files<cr>", "Find File" },
      g = { "<cmd>Telescope live_grep theme=ivy<cr>", "Find Text" },
      o = { "<cmd>Telescope oldfiles theme=ivy<cr>", "Old Files" },
      h = { "<cmd>Telescope help_tags theme=ivy<cr>", "Help Tags" },
      m = { ":Move ", "Move" },
      d = { ":Delete<CR>", "Delete" },
      r = { ":Rename ", "Rename" },
      c = { ":Chmod ", "Chmod" },
      k = { ":Mkdir ", "Mkdir" },
      w = { ":cmd>w!<CR>", "Save" },
      W = { ":SudoWrite<CR>", "SudoWrite" },
      E = { ":SudoEdit ", "SudoEdit" },
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

return F_FILES
