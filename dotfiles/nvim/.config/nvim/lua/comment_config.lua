require('nvim_comment').setup()
vim.api.nvim_set_keymap("n", "<leader>c/", ":CommentToggle<CR>", {noremap=true, silent = true})
vim.api.nvim_set_keymap("v", "<leader>c/", ":CommentToggle<CR>", {noremap=true, silent = true})
