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

vim.g.mapleader = " "
vim.opt.clipboard = "unnamedplus"
vim.opt.signcolumn = "yes"
vim.diagnostic.config({
	underline = true,
	virtual_text = true,
})
