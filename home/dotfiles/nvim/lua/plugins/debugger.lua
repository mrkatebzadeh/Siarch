return {
  "mfussenegger/nvim-dap",
  dependencies = {
    "leoluz/nvim-dap-go",
    "rcarriga/nvim-dap-ui",
    "tpope/vim-fugitive",
  },
  config = function()
    require("dapui").setup()
    require("dap-go").setup()

    local dap, dapui = require("dap"), require("dapui")
    local mason_path = vim.fn.glob(vim.fn.stdpath("data") .. "/mason/")
    local function sep_os_replacer(str)
      local result = str
      local path_sep = package.config:sub(1, 1)
      result = result:gsub("/", path_sep)
      return result
    end

    dap.listeners.before.attach.dapui_config = function()
      dapui.open()
    end
    dap.listeners.before.launch.dapui_config = function()
      dapui.open()
    end
    dap.listeners.before.event_terminated.dapui_config = function()
      dapui.close()
    end
    dap.listeners.before.event_exited.dapui_config = function()
      dapui.close()
    end

    dap.configurations.lua = {
      {
        type = "nlua",
        request = "attach",
        name = "Neovim attach",
        host = function()
          local value = vim.fn.input("Host [127.0.0.1]: ")
          if value ~= "" then
            return value
          end
          return "127.0.0.1"
        end,
        port = function()
          local val = tonumber(vim.fn.input("Port: "))
          assert(val, "Please provide a port number")
          return val
        end,
      },
    }

    local path = vim.fn.glob(mason_path .. "packages/codelldb/extension/")
        or vim.fn.expand("~/") .. ".vscode/extensions/vadimcn.vscode-lldb-1.8.1/"
    local lldb_cmd = path .. "adapter/codelldb"

    dap.adapters.codelldb = {
      type = "server",
      port = "${port}",
      executable = {
        -- CHANGE THIS to your path!
        command = lldb_cmd,
        args = { "--port", "${port}" },
        -- On windows you may have to uncomment this:
        -- detached = false,
      },
    }

    dap.configurations.cpp = {
      {
        name = "Launch file",
        type = "codelldb",
        request = "launch",
        program = function()
          return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file")
        end,
        cwd = "${workspaceFolder}",
        stopOnEntry = true,
      },
    }
    dap.configurations.c = dap.configurations.cpp
    -- dap.configurations.rust = dap.configurations.cpp

    dap.configurations.python = dap.configurations.python or {}
    table.insert(dap.configurations.python, {
      type = "python",
      request = "launch",
      name = "launch with options",
      program = "${file}",
      python = function()
      end,
      pythonPath = function()
        local path
        for _, server in pairs(vim.lsp.buf_get_clients()) do
          if server.name == "pyright" or server.name == "pylance" then
            path = vim.tbl_get(server, "config", "settings", "python", "pythonPath")
            break
          end
        end
        path = vim.fn.input("Python path: ", path or "", "file")
        return path ~= "" and vim.fn.expand(path) or nil
      end,
      args = function()
        local args = {}
        local i = 1
        while true do
          local arg = vim.fn.input("Argument [" .. i .. "]: ")
          if arg == "" then
            break
          end
          args[i] = arg
          i = i + 1
        end
        return args
      end,
      justMyCode = function()
        local yn = vim.fn.input("justMyCode? [y/n]: ")
        if yn == "y" then
          return true
        end
        return false
      end,
      stopOnEntry = function()
        local yn = vim.fn.input("stopOnEntry? [y/n]: ")
        if yn == "y" then
          return true
        end
        return false
      end,
      console = "integratedTerminal",
    })
  end,
}
