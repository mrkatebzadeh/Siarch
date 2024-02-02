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
    -- NOTE: if you want to use `dap` instead of `RustDebuggables` you can use the following configuration

    dap.adapters.go = function(callback, _)
      local stdout = vim.loop.new_pipe(false)
      local handle
      local pid_or_err
      local port = 38697
      local opts = {
        stdio = { nil, stdout },
        args = { "dap", "-l", "127.0.0.1:" .. port },
        detached = true,
      }
      handle, pid_or_err = vim.loop.spawn("dlv", opts, function(code)
        stdout:close()
        handle:close()
        if code ~= 0 then
          print("dlv exited with code", code)
        end
      end)
      assert(handle, "Error running dlv: " .. tostring(pid_or_err))
      stdout:read_start(function(err, chunk)
        assert(not err, err)
        if chunk then
          vim.schedule(function()
            require("dap.repl").append(chunk)
          end)
        end
      end)
      -- Wait for delve to start
      vim.defer_fn(function()
        callback({ type = "server", host = "127.0.0.1", port = port })
      end, 100)
    end
    -- https://github.com/go-delve/delve/blob/master/Documentation/usage/dlv_dap.md
    dap.configurations.go = {
      {
        type = "go",
        name = "Debug",
        request = "launch",
        program = "${file}",
      },
      {
        type = "go",
        name = "Debug test", -- configuration for debugging test files
        request = "launch",
        mode = "test",
        program = "${file}",
      },
      -- works with go.mod packages and sub packages
      {
        type = "go",
        name = "Debug test (go.mod)",
        request = "launch",
        mode = "test",
        program = "./${relativeFileDirname}",
      },
    }

    dap.configurations.dart = {
      {
        type = "dart",
        request = "launch",
        name = "Launch flutter",
        dartSdkPath = sep_os_replacer("/opt/flutter/bin/cache/dart-sdk/"),
        flutterSdkPath = sep_os_replacer("opt/flutter"),
        program = sep_os_replacer("${workspaceFolder}/lib/main.dart"),
        cwd = "${workspaceFolder}",
      },
    }
    local firefox_path = mason_path .. "packages/firefox-debug-adapter/"

    dap.adapters.firefox = {
      type = "executable",
      command = "node",
      args = {
        firefox_path .. "dist/adapter.bundle.js",
      },
    }

    local firefoxExecutable = "/usr/bin/firefox"
    if vim.fn.has("mac") == 1 then
      firefoxExecutable = "/Applications/Firefox.app/Contents/MacOS/firefox"
    end
    local custom_adapter = "pwa-node-custom"
    dap.adapters[custom_adapter] = function(cb, config)
      if config.preLaunchTask then
        local async = require("plenary.async")
        local notify = require("notify").async

        async.run(function()
          ---@diagnostic disable-next-line: missing-parameter
          notify("Running [" .. config.preLaunchTask .. "]").events.close()
        end, function()
          vim.fn.system(config.preLaunchTask)
          config.type = "pwa-node"
          dap.run(config)
        end)
      end
    end

    dap.configurations.typescript = {
      {
        type = "node2",
        name = "node attach",
        request = "attach",
        program = "${file}",
        cwd = vim.fn.getcwd(),
        sourceMaps = true,
        protocol = "inspector",
      },
      {
        type = "chrome",
        name = "Debug with Chrome",
        request = "attach",
        program = "${file}",
        -- cwd = "${workspaceFolder}",
        -- protocol = "inspector",
        port = 9222,
        webRoot = "${workspaceFolder}",
        -- sourceMaps = true,
        sourceMapPathOverrides = {
          -- Sourcemap override for nextjs
          ["webpack://_N_E/./*"] = "${webRoot}/*",
          ["webpack:///./*"] = "${webRoot}/*",
        },
      },
      {
        name = "Debug with Firefox",
        type = "firefox",
        request = "launch",
        reAttach = true,
        sourceMaps = true,
        url = "http://localhost:6969",
        webRoot = "${workspaceFolder}",
        firefoxExecutable = firefoxExecutable,
      },
      {
        name = "Launch",
        type = "pwa-node",
        request = "launch",
        program = "${file}",
        rootPath = "${workspaceFolder}",
        cwd = "${workspaceFolder}",
        sourceMaps = true,
        skipFiles = { "<node_internals>/**" },
        protocol = "inspector",
        console = "integratedTerminal",
      },
      {
        name = "Attach to node process",
        type = "pwa-node",
        request = "attach",
        rootPath = "${workspaceFolder}",
        processId = require("dap.utils").pick_process,
      },
      {
        name = "Debug Main Process (Electron)",
        type = "pwa-node",
        request = "launch",
        program = "${workspaceFolder}/node_modules/.bin/electron",
        args = {
          "${workspaceFolder}/dist/index.js",
        },
        outFiles = {
          "${workspaceFolder}/dist/*.js",
        },
        resolveSourceMapLocations = {
          "${workspaceFolder}/dist/**/*.js",
          "${workspaceFolder}/dist/*.js",
        },
        rootPath = "${workspaceFolder}",
        cwd = "${workspaceFolder}",
        sourceMaps = true,
        skipFiles = { "<node_internals>/**" },
        protocol = "inspector",
        console = "integratedTerminal",
      },
      {
        name = "Compile & Debug Main Process (Electron)",
        type = custom_adapter,
        request = "launch",
        preLaunchTask = "npm run build-ts",
        program = "${workspaceFolder}/node_modules/.bin/electron",
        args = {
          "${workspaceFolder}/dist/index.js",
        },
        outFiles = {
          "${workspaceFolder}/dist/*.js",
        },
        resolveSourceMapLocations = {
          "${workspaceFolder}/dist/**/*.js",
          "${workspaceFolder}/dist/*.js",
        },
        rootPath = "${workspaceFolder}",
        cwd = "${workspaceFolder}",
        sourceMaps = true,
        skipFiles = { "<node_internals>/**" },
        protocol = "inspector",
        console = "integratedTerminal",
      },
      {
        type = "pwa-node",
        request = "launch",
        name = "Debug Jest Tests",
        -- trace = true, -- include debugger info
        runtimeExecutable = "node",
        runtimeArgs = {
          "./node_modules/jest/bin/jest.js",
          "--runInBand",
        },
        rootPath = "${workspaceFolder}",
        cwd = "${workspaceFolder}",
        console = "integratedTerminal",
        internalConsoleOptions = "neverOpen",
      },
    }

    dap.configurations.typescriptreact = dap.configurations.typescript
    dap.configurations.javascript = dap.configurations.typescript
    dap.configurations.javascriptreact = dap.configurations.typescript

    --Java debugger adapter settings
    dap.configurations.java = {
      {
        name = "Debug (Attach) - Remote",
        type = "java",
        request = "attach",
        hostName = "127.0.0.1",
        port = 5005,
      },
      {
        name = "Debug Non-Project class",
        type = "java",
        request = "launch",
        program = "${file}",
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
    dap.configurations.rust = dap.configurations.cpp

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
    dap.adapters.php = {
      type = "executable",
      command = "node",
      args = { mason_path .. "packages/php-debug-adapter/extension/out/phpDebug.js" },
    }
    dap.configurations.php = {
      {
        name = "Listen for Xdebug",
        type = "php",
        request = "launch",
        port = 9003,
      },
      {
        name = "Debug currently open script",
        type = "php",
        request = "launch",
        port = 9003,
        cwd = "${fileDirname}",
        program = "${file}",
        runtimeExecutable = "php",
      },
    }
  end,
}
