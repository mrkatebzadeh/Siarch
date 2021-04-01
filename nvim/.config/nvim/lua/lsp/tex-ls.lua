require'lspconfig'.texlab.setup{
    cmd = {DATA_PATH .. "/lspinstall/latex/texlab"},
	filetypes = {"plaintex", "tex", "bib"},
    on_attach = require'lsp'.common_on_attach,
}
