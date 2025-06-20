local denopsSrc = "~/.cache/dpp/repos/github.com/vim-denops/denops.vim"
local dppSrc = "~/.cache/dpp/repos/github.com/Shougo/dpp.vim"
local dppInstaller = "~/.cache/dpp/repos/github.com/Shougo/dpp-ext-installer"
local dppGit = "~/.cache/dpp/repos/github.com/Shougo/dpp-protocol-git"

vim.opt.runtimepath:append(denopsSrc)
vim.opt.runtimepath:append(dppSrc)
vim.opt.runtimepath:append(dppInstaller)
vim.opt.runtimepath:append(dppGit)

local dpp = require("dpp")

local dppBase = "~/.cache/dpp"
local dppConfig = "~/.config/nvim/dpp.ts"

if dpp.load_state(dppBase) then
	vim.api.nvim_create_autocmd("User", {
		pattern = "DenopsReady",
		callback = function()
			vim.notify("dpp load_state() is failed")
			dpp.make_state(dppBase, dppConfig)
		end,
	})
end

vim.api.nvim_create_autocmd("User", {
	pattern = "Dpp:makeStatePost",
	callback = function()
		vim.notify("dpp make_state() is done")
	end,
})

vim.cmd("filetype indent plugin on")
vim.cmd("syntax on")

vim.cmd.colorscheme("zenbones")

vim.opt.clipboard = "unnamedplus"

vim.opt.signcolumn = "yes"

vim.g.mapleader = " "

vim.diagnostic.config({
  virtual_text = true,
  signs = true,
  underline = true,
  update_in_insert = false,
  severity_sort = true,
})

vim.api.nvim_create_autocmd("FileType", {
	pattern = "go",
	callback = function()
		vim.bo.tabstop = 4
		vim.bo.shiftwidth = 4
		vim.bo.expandtab = false
	end,
})

vim.lsp.config("gopls", {
  cmd = { "gopls" },
  filetypes = { "go", "gomod", "gowork", "gotmpl" },
  root_dir = vim.fs.dirname(
    vim.fs.find({ "go.work", "go.mod", ".git" }, { upward = true })[1]
  ),
  settings = {
    gopls = {
      analyses = {
        unusedparams = true,
        shadow = true,
      },
      staticcheck = true,
      gofumpt = true,
    },
  },
  on_attach = function(client, bufnr)
	  local opts = { buffer = bufnr, silent = true, noremap = true }

	  vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
	  vim.keymap.set("n", "gD", vim.lsp.buf.type_definition, opts)
	  vim.keymap.set("n", "gr", vim.lsp.buf.references, opts)
	  vim.keymap.set("n", "gi", vim.lsp.buf.implementation, opts)
	  vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)
	  vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, opts)
	  vim.keymap.set("n", "<leader>?", vim.lsp.buf.format, opts)
	  vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, opts)
  end,
})
vim.lsp.enable("gopls")
