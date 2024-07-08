local status_ok, which_key = pcall(require, "which-key")
if not status_ok then
  return
end

local opts = {
  mode = "n",    -- NORMAL mode
  prefix = "<leader>",
  buffer = nil,  -- Global mappings. Specify a buffer number for buffer local mappings
  silent = true, -- use `silent` when creating keymaps
  noremap = true, -- use `noremap` when creating keymaps
  nowait = true, -- use `nowait` when creating keymaps
}

local mappings = {
  k = {
    name = "+C/C++",
    c = { ":Task start cmake configure<Cr>", "Configure" },
    t = { ":Task set_module_param cmake target<Cr>", "Set Target" },
    m = { "<CMD>lua require('cppman').open_cppman_for(vim.fn.expand('<cword>'))<CR>", "CPP Man Word" },
    M = { "<CMD>lua require('cppman').input()<CR>", "CPP Man" },
    p = { ":Task set_task_param cmake run ", "Set Parameters" },
    r = { ":Task start cmake run<Cr>", "Run" },
    d = { ":Task start cmake debug<Cr>", "Debug" },
    b = { ":Task start cmake build<Cr>", "Build" },
    B = { ":Task start cmake build_all<Cr>", "Build All" },
    C = { ":Task start cmake clean<Cr>", "Clean" },
    s = { ":ClangdSwitchSourceHeader<cr>", "Header/Src" },
    h = { "<cmd>HeaderguardAdd<Cr>", "Add Headerguard" },
  },
}

which_key.register(mappings, opts)
