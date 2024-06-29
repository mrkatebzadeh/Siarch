local status_ok, which_key = pcall(require, "which-key")
if not status_ok then
  return
end

function my_quit()
  local bufnr = vim.api.nvim_get_current_buf()
  local buf_windows = vim.call("win_findbuf", bufnr)
  local modified = vim.api.nvim_get_option_value("modified", { buf = bufnr })
  if modified and #buf_windows == 1 then
    vim.ui.input({
      prompt = "Unwritten changes. Do you want to quit? (y/n) ",
    }, function(input)

      if input == "y" then
        vim.cmd("qa!")
      end
    end)
  else
    vim.cmd("qa!")
  end
end

local setup = {
  plugins = {
    marks = true,
    registers = true,
    spelling = {
      enabled = true,
      suggestions = 20,
    },
    presets = {
      operators = false,
      motions = true,
      text_objects = true,
      windows = true,
      nav = true,
      z = true,
      g = true,
    },
  },
  key_labels = {
    -- override the label used to display some keys. It doesn't effect WK in any other way.
    -- For example:
    -- ["<space>"] = "SPC",
    -- ["<cr>"] = "RET",
    -- ["<tab>"] = "TAB",
  },
  icons = {
    breadcrumb = "»",
    separator = "->",
    group = "+",
  },
  popup_mappings = {
    scroll_down = "<c-d>",
    scroll_up = "<c-u>",
  },
  window = {
    border = "rounded",
    position = "bottom",
    margin = { 1, 0, 1, 0 },
    padding = { 2, 2, 2, 2 },
    winblend = 0,
  },
  layout = {
    height = { min = 4, max = 25 },
    width = { min = 20, max = 50 },
    spacing = 3,
    align = "left",
  },
  ignore_missing = true,
  hidden = { "<silent>", "<cmd>", "<Cmd>", "<CR>", "call", "lua", "^:", "^ " },
  show_help = true,
  triggers = "auto",
  triggers_blacklist = {
    i = { "j", "k" },
    v = { "j", "k" },
  },
}

local opts = {
  mode = "n",
  prefix = "<leader>",
  buffer = nil,
  silent = true,
  noremap = true,
  nowait = true,
}

local mappings = {
  A = { ":Alpha<CR>", "Alpha" },
  q = { "<cmd>lua my_quit()<CR>", "Quit" },
  P = { "<cmd>lua require('telescope').extensions.projects.projects()<cr>", "Projects" },
  H = {"<cmd>nohlsearch<cr>", "No Highlight"},
}

which_key.setup(setup)
which_key.register(mappings, opts)



require("bindings.C_chatgpt").setup()
require("bindings.S_session").setup()
require("bindings.T_terminal").setup()
require("bindings.a_assembly").setup()
require("bindings.b_buffers").setup()
require("bindings.comment").setup()
require("bindings.d_debug").setup()
require("bindings.f_files").setup()
require("bindings.g_git").setup()
require("bindings.h_goto").setup()
require("bindings.l_lsp").setup()
require("bindings.n_note").setup()
require("bindings.o_options").setup()
require("bindings.r_refactor").setup()
require("bindings.s_search").setup()
require("bindings.t_test").setup()
require("bindings.w_window").setup()
