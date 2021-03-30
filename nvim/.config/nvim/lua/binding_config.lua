vim.api.nvim_set_keymap('n', '<Space>', '<NOP>', {noremap = true, silent = true})
vim.g.mapleader = ' '

-- better window movement
vim.api.nvim_set_keymap('n', '<C-h>', '<C-w>h', {silent = true})
vim.api.nvim_set_keymap('n', '<C-j>', '<C-w>j', {silent = true})
vim.api.nvim_set_keymap('n', '<C-k>', '<C-w>k', {silent = true})
vim.api.nvim_set_keymap('n', '<C-l>', '<C-w>l', {silent = true})

-- Terminal window navigation
vim.api.nvim_set_keymap('n', '<C-h>', '<C-\\><C-N><C-w>h', {silent = false})
vim.api.nvim_set_keymap('n', '<C-j>', '<C-\\><C-N><C-w>j', {silent = false})
vim.api.nvim_set_keymap('n', '<C-k>', '<C-\\><C-N><C-w>k', {silent = false})
vim.api.nvim_set_keymap('n', '<C-l>', '<C-\\><C-N><C-w>l', {silent = false})
vim.api.nvim_set_keymap('n', '<C-h>', '<C-\\><C-N><C-w>h', {silent = false})
vim.api.nvim_set_keymap('n', '<C-j>', '<C-\\><C-N><C-w>j', {silent = false})
vim.api.nvim_set_keymap('n', '<C-k>', '<C-\\><C-N><C-w>k', {silent = false})
vim.api.nvim_set_keymap('n', '<C-l>', '<C-\\><C-N><C-w>l', {silent = false})

-- resize with arrows
vim.api.nvim_set_keymap('n', '<C-Up>', ':resize -2<CR>', {silent = true})
vim.api.nvim_set_keymap('n', '<C-Down>', ':resize +2<CR>', {silent = true})
vim.api.nvim_set_keymap('n', '<C-Left>', ':vertical resize -2<CR>', {silent = true})
vim.api.nvim_set_keymap('n', '<C-Right>', ':vertical resize +2<CR>', {silent = true})
