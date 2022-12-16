local core_plugins = {
  { "wbthomason/packer.nvim" },

	--- Themes
	
	{ "tomasr/molokai" },

	--- NERD
	
	-- { 'scrooloose/nerdtree' },
	-- { 'jistr/vim-nerdtree-tabs' },
  -- { 'Xuyuanp/nerdtree-git-plugin' },
 
	--- Tree
  {
    "kyazdani42/nvim-tree.lua",
    -- event = "BufWinOpen",
    -- cmd = "NvimTreeToggle",
    config = function()
      require("lvim.core.nvimtree").setup()
    end,
  },


  --- Comments

  {
    "numToStr/Comment.nvim",
    event = "BufRead",
    config = function()
      require("lvim.core.comment").setup()
    end,
  },

	--- Buffer & Status

	-- { "vim-airline/vim-airline" },
  -- { "vim-airline/vim-airline-themes" }, 
	{ "glepnir/galaxyline.nvim" },
	{"romgrk/barbar.nvim" },

	--- Icons

  {
    "kyazdani42/nvim-web-devicons",
  },
}

local plugin_loader = {}

local in_headless = #vim.api.nvim_list_uis() == 0

function join_paths(...)
  local result = table.concat({ ... }, path_sep)
  return result
end

function get_runtime_dir()
  return vim.call("stdpath", "data")
end

function get_config_dir()
  return vim.call("stdpath", "config")
end

function get_cache_dir()
	return vim.call("stdpath", "cache")
end

function get_base_dir()
	local base_dir = "~/.config/nvim"
  return base_dir
end
local compile_path = join_paths(get_config_dir(), "plugin", "packer_compiled.lua")
local snapshot_path = join_paths(get_cache_dir(), "snapshots")
local default_snapshot = join_paths(get_base_dir(), "snapshots", "default.json")

function is_directory(path)
  local stat = uv.fs_stat(path)
  return stat and stat.type == "directory" or false
end

function is_file(path)
  local stat = uv.fs_stat(path)
  return stat and stat.type == "file" or false
end

local Hooks = {}

function Hooks.run_pre_update()
end

function Hooks.run_pre_reload()
end

function Hooks.run_on_packer_complete()
  vim.api.nvim_exec_autocmds("User", { pattern = "PackerComplete" })

  -- -- FIXME(kylo252): nvim-tree.lua/lua/nvim-tree/view.lua:442: Invalid window id
  -- vim.g.colors_name = lvim.colorscheme
  -- pcall(vim.cmd.colorscheme, lvim.colorscheme)

  if Hooks._reload_triggered then
    Hooks._reload_triggered = nil
  end
end

function Hooks.run_post_reload()
  Hooks._reload_triggered = true
end

---Reset any startup cache files used by Packer and Impatient
---It also forces regenerating any template ftplugin files
---Tip: Useful for clearing any outdated settings
function Hooks.reset_cache()
  vim.cmd [[LuaCacheClear]]
  plugin_loader.recompile()
  local lvim_modules = {}
  for module, _ in pairs(package.loaded) do
    if module:match "lvim.core" or module:match "lvim.lsp" then
      package.loaded[module] = nil
      table.insert(lvim_modules, module)
    end
  end
  require("lvim.lsp.templates").generate_templates()
end

function Hooks.run_post_update()
  if vim.fn.has "nvim-0.8" ~= 1 then
    local compat_tag = "1.1.4"
    vim.notify(
      "Please upgrade your Neovim base installation to v0.7+",
      vim.log.levels.WARN
    )
    vim.wait(1000, function()
      return false
    end)
    local ret = reload("lvim.utils.git").switch_lvim_branch(compat_tag)
    if ret then
      vim.notify("Reverted to the last known compatible version: " .. compat_tag, vim.log.levels.WARN)
    end
    return
  end

  Hooks.reset_cache()

  plugin_loader.sync_core_plugins()

  if not in_headless then
    vim.schedule(function()
      if package.loaded["nvim-treesitter"] then
        vim.cmd [[ TSUpdateSync ]]
      end
      -- TODO: add a changelog
      vim.notify("Update complete", vim.log.levels.INFO)
    end)
  end
end


function plugin_loader.init(opts)
  opts = opts or {}

  local install_path = opts.install_path
    or join_paths(vim.fn.stdpath "data", "site", "pack", "packer", "start", "packer.nvim")

  local max_jobs = 100
  if vim.fn.has "mac" == 1 then
    max_jobs = 50
  end

  local init_opts = {
    package_root = opts.package_root or join_paths(vim.fn.stdpath "data", "site", "pack"),
    compile_path = compile_path,
    snapshot_path = snapshot_path,
    max_jobs = max_jobs,
    log = { level = "warn" },
    git = {
      clone_timeout = 120,
    },
    display = {
      open_fn = function()
        return require("packer.util").float { border = "rounded" }
      end,
    },
  }

  if in_headless then
    init_opts.display = nil
    init_opts.git.clone_timeout = 300
  end

  if not is_directory(install_path) then
    print "Initializing first time setup"
    print "Installing packer"
    print(vim.fn.system { "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path })
    vim.cmd "packadd packer.nvim"
  end

  local status_ok, packer = pcall(require, "packer")
  if status_ok then
    packer.on_complete = vim.schedule_wrap(function()
      Hooks.run_on_packer_complete()
    end)
    packer.init(init_opts)
  end
end

-- packer expects a space separated list
local function pcall_packer_command(cmd, kwargs)
  local status_ok, msg = pcall(function()
    require("packer")[cmd](unpack(kwargs or {}))
  end)
  if not status_ok then
  end
end

function plugin_loader.cache_clear()
  if not is_file(compile_path) then
    return
  end
  if vim.fn.delete(compile_path) == 0 then
  end
end

function plugin_loader.compile()
  vim.api.nvim_create_autocmd("User", {
    pattern = "PackerCompileDone",
    once = true,
    callback = function()
      if is_file(compile_path) then
      end
    end,
  })
  pcall_packer_command "compile"
end

function plugin_loader.recompile()
  plugin_loader.cache_clear()
  plugin_loader.compile()
end

function plugin_loader.reload(configurations)
  _G.packer_plugins = _G.packer_plugins or {}
  for k, v in pairs(_G.packer_plugins) do
    if k ~= "packer.nvim" then
      _G.packer_plugins[v] = nil
    end
  end
  plugin_loader.load(configurations)

  plugin_loader.ensure_plugins()
end

function plugin_loader.load()
  local packer_available, packer = pcall(require, "packer")
  if not packer_available then
    return
  end
  local status_ok, _ = xpcall(function()
    packer.reset()
    packer.startup(function(use)
      for _, plugins in ipairs(core_plugins) do
        for _, plugin in ipairs(plugins) do
          use(plugin)
        end
      end
    end)
  end, debug.traceback)

  if not status_ok then
  end
end

function plugin_loader.get_core_plugins()
  local list = {}
  local plugins = core_plugins
  for _, item in pairs(plugins) do
    if not item.disable then
      table.insert(list, item[1]:match "/(%S*)")
    end
  end
  return list
end

function plugin_loader.load_snapshot(snapshot_file)
  snapshot_file = snapshot_file or default_snapshot
  if not in_headless then
    vim.notify("Syncing core plugins is in progress..", vim.log.levels.INFO, { title = "lvim" })
  end
  local core_plugins = plugin_loader.get_core_plugins()
  require("packer").rollback(snapshot_file, unpack(core_plugins))
end

function plugin_loader.sync_core_plugins()
  plugin_loader.cache_clear()
  local core_plugins = plugin_loader.get_core_plugins()
  pcall_packer_command("sync", core_plugins)
end

function plugin_loader.ensure_plugins()
  vim.api.nvim_create_autocmd("User", {
    pattern = "PackerComplete",
    once = true,
    callback = function()
      plugin_loader.compile()
    end,
  })
  pcall_packer_command "install"
end

return plugin_loader
