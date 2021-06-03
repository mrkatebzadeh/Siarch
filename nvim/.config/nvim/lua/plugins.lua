local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
  execute('!git clone https://github.com/wbthomason/packer.nvim '..install_path)
  execute 'packadd packer.nvim'
end

return require('packer').startup(function()
  -- Packer can manage itself
  use 'wbthomason/packer.nvim'

  -- Theme
  use 'tomasr/molokai'
  use {'dracula/vim', as = 'dracula'}

  -- NERD
  --use {'scrooloose/nerdtree'}
  --use {'jistr/vim-nerdtree-tabs'}
  --use {'Xuyuanp/nerdtree-git-plugin'}
  use 'ryanoasis/vim-devicons'
  use 'kyazdani42/nvim-web-devicons'

  -- Tree
  use 'kyazdani42/nvim-tree.lua'

  -- Comment
  use 'terrortylor/nvim-comment'

  -- Buffer & Status
  --use 'vim-airline/vim-airline'
  --use 'vim-airline/vim-airline-themes'
  use 'glepnir/galaxyline.nvim'
  use 'romgrk/barbar.nvim'

  -- Git
  use {'lewis6991/gitsigns.nvim', requires = {'nvim-lua/plenary.nvim'}}
  use 'tpope/vim-rhubarb'
  use 'airblade/vim-gitgutter'
  use 'tpope/vim-fugitive'
  --use 'TimUntersberger/neogit'
  use 'f-person/git-blame.nvim'

  -- General
  use 'vim-scripts/grep.vim'
  use 'vim-scripts/CSApprox'
  use 'bronson/vim-trailing-whitespace'
  use 'Raimondi/delimitMate'
  use 'majutsushi/tagbar'
  use 'Yggdroot/indentLine'
  use 'avelino/vim-bootstrap-updater'
  use 'junegunn/goyo.vim'
  use 'jackguo380/vim-lsp-cxx-highlight'
  use 'vim-syntastic/syntastic'
  use 'rhysd/vim-clang-format'
  -- use 'liuchengxu/vim-which-key'
  use 'mhinz/vim-startify'
  use 'vimwiki/vimwiki'
  use 'windwp/nvim-autopairs'
  use 'voldikss/vim-floaterm'
  use 'tpope/vim-eunuch'
  use {
    'AckslD/nvim-whichkey-setup.lua',
    requires = {'liuchengxu/vim-which-key'},
  }

  -- Sessions
  use 'xolox/vim-misc'
  use 'xolox/vim-session'

  -- Languages
  --- C
  use {'vim-scripts/c.vim', ft = {'c', 'cpp'}}
  use 'ludwig/split-manpage.vim'
  use 'alpertuna/vim-header'
  --- html
  use 'hail2u/vim-css3-syntax'
  use 'gorodinskiy/vim-coloresque'
  use 'tpope/vim-haml'
  use 'mattn/emmet-vim'
  --- javascript
  use 'jelera/vim-javascript-syntax'
  --- lisp
  use 'vim-scripts/slimv.vim'
  --- python
--  use 'davidhalter/jedi-vim'
  use {'raimon49/requirements.txt.vim', ft = {'requirements'}}

  -- LSP
  use 'neovim/nvim-lspconfig'
  use 'glepnir/lspsaga.nvim'
  use 'onsails/lspkind-nvim'
  use 'kosayoda/nvim-lightbulb'
  use 'mfussenegger/nvim-jdtls'
  use 'kabouzeid/nvim-lspinstall'

  -- FZF
  use 'junegunn/fzf.vim'
  use '/usr/local/opt/fzf'
  
  -- Autocomplete
  use 'hrsh7th/nvim-compe'
  use 'hrsh7th/vim-vsnip'
  use 'hrsh7th/vim-vsnip-integ'
  use "rafamadriz/friendly-snippets"
  use 'ChristianChiarulli/html-snippets'

  -- Treesitter
  use {'nvim-treesitter/nvim-treesitter', run = ':TSUpdate'}
  use 'nvim-treesitter/nvim-treesitter-refactor'
  use 'nvim-treesitter/playground'
  use 'p00f/nvim-ts-rainbow'
  use 'JoosepAlviste/nvim-ts-context-commentstring'
  use 'windwp/nvim-ts-autotag'

  -- Telescope
  use 'nvim-lua/popup.nvim'
  --use 'nvim-lua/plenary.nvim'
  use 'nvim-telescope/telescope.nvim'
  use 'nvim-telescope/telescope-media-files.nvim'

  -- Markdown
  use 'vim-pandoc/vim-pandoc-syntax'
  use 'iamcco/markdown-preview.nvim'
end)
