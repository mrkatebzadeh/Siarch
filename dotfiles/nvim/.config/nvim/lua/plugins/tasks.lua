return {
  "https://github.com/Shatur/neovim-tasks",
  dependencies = { "nvim-lua/plenary.nvim", "mfussenegger/nvim-dap" },
  config = function()
    local Path = require("plenary.path")
    require("tasks").setup({
      default_params = {
        cmake = {
          cmd = "cmake",
          build_dir = tostring(Path:new("{cwd}", "build", "{os}-{build_type}")),
          build_type = "Debug",
          dap_name = "codelldb",
          args = {
            configure = { "-D", "CMAKE_EXPORT_COMPILE_COMMANDS=1" },
          },
        },
      },
      save_before_run = true,
      params_file = "neovim.json",
      quickfix = {
        pos = "botright",
        height = 10,
      },
      dap_open_command = function()
        return require("dap").repl.open()
      end,
    })
  end,
}
