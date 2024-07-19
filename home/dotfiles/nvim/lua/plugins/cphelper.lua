return {
  "p00f/cphelper.nvim",
  config = function()
    vim.g["cph#dir"] = os.getenv('HOME') .. '/Contests'
    vim.g["cph#lang"] = 'rust'
  end,
}
