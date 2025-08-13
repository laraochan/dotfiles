vim.g.mapleader = " "

local denopsSrc = vim.fn.stdpath("cache") .. "/dpp/repos/github.com/vim-denops/denops.vim"
local denopsHello = vim.fn.stdpath("cache") .. "/dpp/repos/github.com/vim-denops/denops-helloworld.vim"

vim.opt.runtimepath:append(denopsSrc)
vim.opt.runtimepath:append(denopsHello)

local dppBase = vim.fn.stdpath("cache") .. "/dpp"
local dppConfig = vim.fn.stdpath("config") .. "/dpp/config.ts"

local dppSrc = vim.fn.stdpath("cache") .. "/dpp/repos/github.com/Shougo/dpp.vim"
local dppInstaller = vim.fn.stdpath("cache") .. "/dpp/repos/github.com/Shougo/dpp-ext-installer"
local dppLazy = vim.fn.stdpath("cache") .. "/dpp/repos/github.com/Shougo/dpp-ext-lazy"
local dppToml = vim.fn.stdpath("cache") .. "/dpp/repos/github.com/Shougo/dpp-ext-toml"
local dppGit = vim.fn.stdpath("cache") .. "/dpp/repos/github.com/Shougo/dpp-protocol-git"

vim.opt.runtimepath:append(dppSrc)
vim.opt.runtimepath:append(dppInstaller)
vim.opt.runtimepath:append(dppLazy)
vim.opt.runtimepath:append(dppToml)
vim.opt.runtimepath:append(dppGit)

local dpp = require("dpp")

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

vim.api.nvim_create_user_command("DppInstall", function()
	vim.fn["dpp#async_ext_action"]("installer", "install")
end, {})
vim.api.nvim_create_user_command("DppUpdate", function(opts)
	local args = opts.fargs
	vim.fn["dpp#async_ext_action"]("installer", "update", { names = args })
end, { nargs = "*" })

vim.o.clipboard = "unnamedplus"
vim.o.smartindent = true
vim.o.ignorecase = true
vim.o.tabstop = 2
vim.o.softtabstop = 2
vim.o.shiftwidth = 2
vim.o.expandtab = true
vim.o.smartcase = true
vim.o.swapfile = false
vim.o.signcolumn = "yes"
vim.diagnostic.config({
	underline = true,
	virtual_text = true,
})

vim.api.nvim_create_autocmd("FileType", {
	pattern = { "lua" },
	callback = function()
		vim.bo.tabstop = 2
		vim.bo.softtabstop = 2
		vim.bo.shiftwidth = 2
		vim.bo.expandtab = true
	end,
})
vim.api.nvim_create_autocmd("FileType", {
  pattern = { "php" },
  callback = function()
    vim.bo.tabstop = 4
    vim.bo.softtabstop = 4
    vim.bo.shiftwidth = 4
    vim.bo.expandtab = true
  end,
})
vim.api.nvim_create_autocmd("FileType", {
  pattern = { "typescript", "typescriptreact", "javascript", "javascriptreact" },
  callback = function()
    vim.bo.tabstop = 2
    vim.bo.softtabstop = 2
    vim.bo.shiftwidth = 2
    vim.bo.expandtab = true
  end,
})
vim.api.nvim_create_autocmd("FileType", {
  pattern = { "go" },
  callback = function()
    vim.bo.tabstop = 4
    vim.bo.softtabstop = 4
    vim.bo.shiftwidth = 4
    vim.bo.expandtab = false
  end,
})
vim.api.nvim_create_autocmd("FileType", {
  pattern = { "rust" },
  callback = function()
    vim.bo.tabstop = 4
    vim.bo.softtabstop = 4
    vim.bo.shiftwidth = 4
    vim.bo.expandtab = true
  end,
})
