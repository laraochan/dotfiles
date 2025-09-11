vim.o.expandtab = true
vim.o.shiftwidth = 2
vim.o.tabstop = 2
vim.o.softtabstop = 2

vim.o.autoindent = true
vim.o.smartindent = true

vim.o.shiftround = true
vim.o.smarttab = true

vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.hlsearch = true
vim.o.incsearch = true

vim.o.clipboard = "unnamedplus"

vim.o.backup = false
vim.o.writebackup = false
vim.o.swapfile = false

vim.o.signcolumn = "yes"
vim.o.background = "dark"
vim.o.termguicolors = true

vim.o.errorbells = false
vim.o.visualbell = false

vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

vim.diagnostic.config({
  virtual_text = {
    prefix = "●",
  },
  virtual_lines = false,
  signs = true,
  float = {
    border = "rounded",
  },
})
