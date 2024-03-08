return {
  {
    "nvim-tree/nvim-web-devicons",
    lazy = true,
  },
  {
    "RRethy/vim-illuminate",
    lazy = true,
    event = "BufReadPost",
    opts = {
      filetypes_denylist = {
        "NvimTree",
      },
    },
    config = function(_, opts)
      require("illuminate").configure(opts)

      local highlight = "#354A51"
      vim.api.nvim_set_hl(0, "IlluminatedWordText", {
        bg = highlight,
      })
      vim.api.nvim_set_hl(0, "IlluminatedWordRead", {
        bg = highlight,
      })
      vim.api.nvim_set_hl(0, "IlluminatedWordWrite", {
        bg = highlight,
      })
    end,
  },
  {
    "cappyzawa/trim.nvim",
    opts = {},
  },
  {
    "utilyre/barbecue.nvim",
    version = "v1.*",
    lazy = true,
    event = "BufReadPre",
    dependencies = {
      "SmiteshP/nvim-navic",
      "nvim-tree/nvim-web-devicons",
    },
    config = true,
  },
  {
    "hedyhli/outline.nvim",
    config = function()
      require("outline").setup({})
    end,
  },
  {
    "norcalli/nvim-colorizer.lua",
  },
  { "onsails/lspkind.nvim" },
  {
    "lukas-reineke/indent-blankline.nvim",
    main = "ibl",
    config = function()
      local highlight = {
        "RainbowRed",
        "RainbowYellow",
        "RainbowBlue",
        "RainbowOrange",
        "RainbowGreen",
        "RainbowViolet",
        "RainbowCyan",
      }

      local hooks = require("ibl.hooks")
      -- create the highlight groups in the highlight setup hook, so they are reset
      -- every time the colorscheme changes
      hooks.register(hooks.type.HIGHLIGHT_SETUP, function()
        vim.api.nvim_set_hl(0, "RainbowRed", { fg = "#E06C75" })
        vim.api.nvim_set_hl(0, "RainbowYellow", { fg = "#E5C07B" })
        vim.api.nvim_set_hl(0, "RainbowBlue", { fg = "#61AFEF" })
        vim.api.nvim_set_hl(0, "RainbowOrange", { fg = "#D19A66" })
        vim.api.nvim_set_hl(0, "RainbowGreen", { fg = "#98C379" })
        vim.api.nvim_set_hl(0, "RainbowViolet", { fg = "#C678DD" })
        vim.api.nvim_set_hl(0, "RainbowCyan", { fg = "#56B6C2" })
      end)

      require("ibl").setup({ indent = { highlight = highlight, char = "│" } })
    end,
  },
  {
    "kylechui/nvim-surround",
    version = "*", -- Use for stability; omit to use `main` branch for the latest features
    event = "VeryLazy",
    config = function()
      require("nvim-surround").setup({
        -- Configuration here, or leave empty to use defaults
      })
    end,
  },
  {
    "folke/todo-comments.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    opts = {
      -- your configuration comes here
      -- or leave it empty to use the default settings
      -- refer to the configuration section below
    },
  },
  {
    "debugloop/telescope-undo.nvim",
    dependencies = { -- note how they're inverted to above example
      {
        "nvim-telescope/telescope.nvim",
        dependencies = { "nvim-lua/plenary.nvim" },
      },
    },
    opts = {
      -- don't use `defaults = { }` here, do this in the main telescope spec
      extensions = {
        undo = {
          -- telescope-undo.nvim config, see below
        },
        -- no other extensions here, they can have their own spec too
      },
    },
    config = function(_, opts)
      -- Calling telescope's setup from multiple specs does not hurt, it will happily merge the
      -- configs for us. We won't use data, as everything is in it's own namespace (telescope
      -- defaults, as well as each extension).
      require("telescope").setup(opts)
      require("telescope").load_extension("undo")
    end,
  },
  {
    "SmiteshP/nvim-navic",
    config = function()
      require("nvim-navic").setup()
    end,
  },
  --[[ {
    "sunjon/shade.nvim",
    config = function()
      require("shade").setup({
        overlay_opacity = 60,
        opacity_step = 1,
      })
    end,
  }, ]]
}
