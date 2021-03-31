vim.o.fileencoding = "utf-8"
vim.cmd('set colorcolumn=99999')
vim.o.mouse = "a"
vim.o.splitbelow = true
vim.o.splitright = true
vim.o.conceallevel = 0
vim.cmd('set ts=4')
vim.cmd('set sw=4')
vim.bo.expandtab = true
vim.bo.smartindent = true
vim.o.backup = false
vim.o.writebackup = false
vim.wo.signcolumn = "yes"
vim.o.updatetime = 300
vim.o.timeoutlen = 100
vim.o.clipboard = "unnamedplus"
vim.o.hidden = true
vim.o.hlsearch = true
vim.o.incsearch = true
vim.o.ignorecase = true
vim.o.smartcase = true

vim.g.CSApprox_loaded = 1

-- session management
vim.g.session_directory = "~/.config/nvim/session"
vim.g.session_autoload = "no"
vim.g.session_autosave = "no"
vim.g.session_command_aliases = 1

-- indentLine
vim.g.indentLine_enabled = 1
vim.g.indentLine_concealcursor = 0
vim.g.indentLine_char = '┆'
vim.g.indentLine_faster = 1

DATA_PATH = vim.fn.stdpath('data')
CACHE_PATH = vim.fn.stdpath('cache')
