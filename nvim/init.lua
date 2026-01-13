vim.g.mapleader = " "

vim.o.expandtab = true
vim.o.tabstop = 2
vim.o.shiftwidth = 2
vim.o.softtabstop = 2

vim.o.autoindent = true
vim.o.smartindent = true
vim.o.smarttab = true
vim.o.shiftround = true

vim.o.breakindent = true

vim.o.number = true
vim.o.signcolumn = "yes"

vim.api.nvim_create_autocmd("FileType", {
	pattern = { "go" },
	callback = function()
		vim.bo.expandtab = false
		vim.bo.tabstop = 4
		vim.bo.shiftwidth = 4
	end,
})

vim.pack.add({
	{
		src = "https://github.com/nvim-treesitter/nvim-treesitter",
		version = "main",
	},
	{
		src = "https://github.com/folke/tokyonight.nvim",
		version = "main",
	},
  {
    src = "https://github.com/nvim-lua/plenary.nvim",
    version = "master"
  },
	{
		src = "https://github.com/saghen/blink.cmp",
		version = "main",
	},
	{
		src = "https://github.com/lewis6991/gitsigns.nvim",
		version = "main",
	},
	{
		src = "https://github.com/stevearc/oil.nvim",
		version = "master",
	},
	{
		src = "https://github.com/nvim-mini/mini.icons",
		version = "main",
	},
  {
    src = "https://github.com/nvim-telescope/telescope.nvim",
    version = "master"
  }
})

vim.api.nvim_create_autocmd("PackChanged", {
	callback = function(ev)
		local name, kind = ev.data.spec.name, ev.data.kind
		
		if name == "nvim-treesitter" and (kind == "install" or kind == "update") then
			vim.cmd.TSUpdate()
		end
	end,
})
require("nvim-treesitter").install({ "lua", "vim", "vimdoc", "typescript", "tsx", "jsdoc", "json", "toml", "yaml", "php", "phpdoc", "html", "css", "scss" })
vim.api.nvim_create_autocmd("FileType", {
	pattern = { "lua", "vim", "typescript", "typescriptreact", "json", "toml", "yaml", "php", "html", "css", "scss" },
	callback = function()
		vim.treesitter.start()
	end,
})

require("tokyonight").setup({
	style = "storm",
	transparent = true,
	terminal_colors = true,
})
vim.cmd.colorscheme("tokyonight")

require("blink.cmp").setup({
	keymap = {
		preset = "default"
	},
	appearance = {
		nerd_font_variant = "mono",
	},
	completion = { documentation = { auto_show = true } },
	sources = {
		default = { "lsp", "path", "buffer" },
	},
	fuzzy = { implementation = "lua" },
})

require("gitsigns").setup()

require("oil").setup({
  default_file_explorer = true,
	columns = {
		"icon",
	},
	keymaps = {
		["q"] = { "actions.close", mode = "n" }
	},
})
vim.keymap.set("n", "-", "<CMD>Oil<CR>", { desc = "Open parent directory" })

require("mini.icons").setup()

local builtin = require('telescope.builtin')
vim.keymap.set('n', '<leader>ff', builtin.find_files, { desc = 'Telescope find files' })
vim.keymap.set('n', '<leader>fg', builtin.live_grep, { desc = 'Telescope live grep' })
vim.keymap.set('n', '<leader>fb', builtin.buffers, { desc = 'Telescope buffers' })
vim.keymap.set('n', '<leader>fh', builtin.help_tags, { desc = 'Telescope help tags' })
