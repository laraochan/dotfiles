local denopsSrc = "~/.cache/dpp/repos/github.com/vim-denops/denops.vim"
local denopsHello = "~/.cache/dpp/repos/github.com/vim-denops/denops-helloworld.vim"

local dppSrc = "~/.cache/dpp/repos/github.com/Shougo/dpp.vim"
local dppInstaller = "~/.cache/dpp/repos/github.com/Shougo/dpp-ext-installer"
local dppToml = "~/.cache/dpp/repos/github.com/Shougo/dpp-ext-toml"
local dppLazy = "~/.cache/dpp/repos/github.com/Shougo/dpp-ext-lazy"
local dppGit = "~/.cache/dpp/repos/github.com/Shougo/dpp-protocol-git"

local dppBase = "~/.cache/dpp/"
local dppConfig = "~/.config/nvim/dpp.ts"

vim.opt.runtimepath:append(denopsSrc)
vim.opt.runtimepath:append(denopsHello)

vim.opt.runtimepath:append(dppSrc)
vim.opt.runtimepath:append(dppInstaller)
vim.opt.runtimepath:append(dppToml)
vim.opt.runtimepath:append(dppLazy)
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

vim.api.nvim_create_user_command("DppInstall", "call dpp#async_ext_action('installer', 'install')", {})
vim.api.nvim_create_user_command("DppUpdate", function(opts)
	local args = opts.fargs
	vim.fn["dpp#async_ext_action"]("installer", "update", { names=args })
end, { nargs = "*" })
