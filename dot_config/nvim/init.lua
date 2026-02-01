vim.g.mapleader = " "

vim.pack.add({
	"https://github.com/EdenEast/nightfox.nvim",
  "https://github.com/folke/tokyonight.nvim",
  "https://github.com/nvim-tree/nvim-web-devicons",
  "https://github.com/nvim-lua/plenary.nvim",
	"https://github.com/nvim-treesitter/nvim-treesitter",
	"https://github.com/neovim/nvim-lspconfig",
  "https://github.com/stevearc/oil.nvim",
  "https://github.com/nvim-telescope/telescope.nvim",
  "https://github.com/saghen/blink.cmp",
  "https://github.com/nvimdev/dashboard-nvim",
})

-- dashboard-nvim
require("dashboard").setup()

-- nightfox
require("nightfox").setup({
	options = {
		transparent = true,
	},
})
vim.cmd.colorscheme("duskfox")

-- nvim-treesitter
vim.api.nvim_create_autocmd("PackChanged", {
	callback = function(ev)
		local name, kind = ev.data.spec.name, ev.data.kind
		if name == "nvim-treesitter" and (kind == "install" or kind == "update") then
			vim.cmd(":TSUpdate")
		end
	end,
})
require("nvim-treesitter").install({ "lua", "vim", "vimdoc", "tsx", "typescript", "json", "toml", "html", "css" })
vim.api.nvim_create_autocmd("FileType", {
  pattern = { "lua", "vim", "typescript", "typescriptreact", "javascript", "javascriptreact", "json", "toml", "html", "css" },
  callback = function() vim.treesitter.start() end,
})

-- blink.cmp
require("blink.cmp").setup({
  keymap = { preset = "default" },
  appearance = {
    nerd_font_variant = "mono",
  },
  completion = { documentation = { auto_show = true } },
  sources = {
    default = { "lsp", "path", "buffer" },
  },
  fuzzy = { implementation = "lua" },
})

-- nvim-lspconfig
vim.keymap.del({ "n", "x" }, "gra")
vim.keymap.del("n", "gri")
vim.keymap.del("n", "grn")
vim.keymap.del("n", "grr")
vim.keymap.del("n", "grt")
vim.keymap.del("n", "gO")
vim.keymap.del("i", "<C-s>")
vim.keymap.del({ "x", "o" }, "an")
vim.keymap.del({ "x", "o" }, "in")
vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("my.lsp", {}),
  callback = function(args)
    vim.keymap.del('n', 'K', { buffer = args.buf })

    local client = assert(vim.lsp.get_client_by_id(args.data.client_id))
    if client:supports_method("textDocument/hover") then
      vim.keymap.set("n", "<Leader>k", vim.lsp.buf.hover, { buffer = args.buf })
    end
    if client:supports_method("textDocument/definition") then
      vim.keymap.set("n", "<Leader>gd", vim.lsp.buf.definition, { buffer = args.buf })
    end
    if client:supports_method("textDocument/typeDefinition") then
      vim.keymap.set("n", "<Leader>gD", vim.lsp.buf.type_definition, { buffer = args.buf })
    end
    if client:supports_method("textDocument/references") then
      vim.keymap.set("n", "<Leader>gr", vim.lsp.buf.references, { buffer = args.buf })
    end
    if client:supports_method("textDocument/implementation") then
      vim.keymap.set("n", "<Leader>gi", vim.lsp.buf.implementation, { buffer = args.buf })
    end
    if client:supports_method("textDocument/rename") then
      vim.keymap.set("n", "<Leader>gR", vim.lsp.buf.rename, { buffer = args.buf })
    end
    if client:supports_method("textDocument/codeAction") then
      vim.keymap.set("n", "<Leader>ca", vim.lsp.buf.code_action, { buffer = args.buf })
    end
  end,
})
vim.lsp.enable({ "vtsls", "intelephense" })

-- oil.nvim
require("oil").setup({
  default_file_explorer = true,
  columns = { "icon" },
  keymaps = {
    ["q"] = "actions.close",
  }
})
vim.keymap.set("n", "-", "<CMD>Oil<CR>", { desc = "Open parent directory" })

-- telescope.nvim
require('telescope').setup({
  defaults = {
    borderchars = { "─", "│", "─", "│", "┌", "┐", "┘", "└" },
  },
})
local telescope_builtin = require('telescope.builtin')
vim.keymap.set('n', '<leader>ff', telescope_builtin.find_files, { desc = 'Telescope find files' })
vim.keymap.set('n', '<leader>fg', telescope_builtin.live_grep, { desc = 'Telescope live grep' })
vim.keymap.set('n', '<leader>fb', telescope_builtin.buffers, { desc = 'Telescope buffers' })
vim.keymap.set('n', '<leader>fh', telescope_builtin.help_tags, { desc = 'Telescope help tags' })

-- options
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.signcolumn = "yes"
vim.opt.cursorline = true
vim.opt.wrap = false
vim.opt.equalalways = false
vim.opt.mouse = "a"

vim.opt.splitright = true
vim.opt.splitbelow = true

vim.opt.exrc = true
vim.opt.secure = true

local tabwidth = 2
vim.opt.expandtab = true
vim.opt.tabstop = tabwidth
vim.opt.softtabstop = tabwidth
vim.opt.shiftwidth = tabwidth

vim.opt.autoindent = true
vim.opt.smartindent = true

vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.incsearch = true
vim.opt.hlsearch = true

vim.opt.undofile = true
vim.opt.undodir = vim.fn.stdpath("data") .. "/undo"

vim.opt.winborder = "single"

-- diagnostic
vim.diagnostic.config({
  virtual_text = {
    format = function(diagnostic)
      return string.format("%s (%s: %s)", diagnostic.message, diagnostic.source, diagnostic.code)
    end,
  },
})

-- borderの背景をhover(=NormalFloat)背景と一致させる
vim.api.nvim_create_autocmd({ "ColorScheme", "VimEnter" }, {
  callback = function()
    local nf = vim.api.nvim_get_hl(0, { name = "NormalFloat", link = false })
    local fb = vim.api.nvim_get_hl(0, { name = "FloatBorder", link = false })

    vim.api.nvim_set_hl(0, "FloatBorder", {
      fg = fb.fg or nf.fg, -- fg は既存があれば尊重
      bg = nf.bg,
    })
  end,
})
