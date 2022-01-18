let blacklist = ['java']
autocmd CursorHold,CursorHoldI * if index(blacklist, &ft) < 0 | lua require'nvim-lightbulb'.update_lightbulb()


" commands
command! LspCodeAction lua require 'nv-utils'.code_action()
command! LspDeclaration lua require 'nv-utils'.declaration()
command! LspDefinition lua require 'nv-utils'.definition()
command! LspDocumentSymbol lua require 'nv-utils'.document_symbol()
command! LspFormatting lua require 'nv-utils'.formatting()
command! LspFormattingSync lua require 'nv-utils'.formatting_sync()
command! LspHover lua require 'nv-utils'.hover()
command! LspImplementation lua require 'nv-utils'.implementation()
command! LspRangeCodeAction lua require 'nv-utils'.range_code_action()
command! LspRangeFormatting lua require 'nv-utils'.range_formatting()
command! LspReferences lua require 'nv-utils'.references()
command! LspRename lua require 'nv-utils'.rename()
command! LspTypeDefinition lua require 'nv-utils'.type_definition()
command! LspWorkspaceSymbol lua require 'nv-utils'.workspace_symbol()
command! LspGotoNext lua require 'nv-utils'.goto_next()
command! LspGotoPrev lua require 'nv-utils'.goto_prev()
command! LspShowLineDiagnostics lua require 'nv-utils'.show_line_diagnostics()
