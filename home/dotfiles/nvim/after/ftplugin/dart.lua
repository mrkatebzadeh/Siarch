local status_ok, which_key = pcall(require, "which-key")
if not status_ok then
  return
end

local opts = {
  mode = "n", -- NORMAL mode
  prefix = "<leader>",
  buffer = nil, -- Global mappings. Specify a buffer number for buffer local mappings
  silent = true, -- use `silent` when creating keymaps
  noremap = true, -- use `noremap` when creating keymaps
  nowait = true, -- use `nowait` when creating keymaps
}

local mappings = {
  k = {
    name = "+Flutter",
    c = { ":FlutterCopyProfilerUrl<CR>", "Copy Profile Url" },
    d = { ":FlutterDevices<CR>", "Devices" },
    D = { ":FlutterDevTools<CR>", "Dev Tools" },
    e = { ":FlutterEmulators<CR>", "Emulators" },
    h = { ":FlutterReload<CR>", "Reload" },
    H = { ":FlutterRestart<CR>", "Restart" },
    l = { ":FlutterLogClear<CR>", "Log Clear" },
    o = { ":FlutterOutline<CR>", "Outline" },
    p = { ":FlutterPubGet<CR>", "Pub Get" },
    q = { ":FlutterQuit<CR>", "Quit" },
    r = { ":FlutterRun<CR>", "Run" },
    v = { ":FlutterVisualDebug<CR>", "Visual Debug" },
  },
}

which_key.register(mappings, opts)
