local pluginsDir = vim.fn.stdpath("config") .. "/plugins/"
local plugins = {
	-- colorscheme
	"rktjmp/lush.nvim",
	"zenbones-theme/zenbones.nvim",

	-- git
	"lewis6991/gitsigns.nvim",
}

if not vim.loop.fs_stat(pluginsDir) then
	vim.notify("make plugins directory...")
	vim.cmd("redraw")
	vim.fn.mkdir(pluginsDir, "p")
	vim.notify("done!")
	vim.cmd("redraw")
end

for _, repo in ipairs(plugins) do
	local name = repo:match("([^/]+)/?$")
	local fullpath = pluginsDir .. name
	if not vim.loop.fs_stat(fullpath) then
		vim.notify("Installing " .. string.format("%s", repo))
		vim.cmd("redraw")
		local clone_cmd = {
			"git",
			"clone",
			"--filter=blob:none",
			"https://github.com/" .. repo,
			fullpath,
		}
		vim.fn.system(clone_cmd)
		vim.notify("Installed " .. string.format("%s", repo))
		vim.cmd("redraw")
	end

	vim.opt.runtimepath:append(pluginsDir .. name)
end

vim.cmd.colorscheme("zenbones")

vim.opt.termguicolors = true
vim.opt.clipboard = "unnamedplus"
vim.opt.swapfile = false
vim.opt.shiftround = true
vim.opt.cursorline = true

vim.opt.signcolumn = "yes"

vim.g.mapleader = " "

vim.diagnostic.config({
	-- virtual_text = true,
	virtual_lines = true,
	underline = true,
	signs = true,
})

vim.api.nvim_create_autocmd("FileType", {
	pattern = "go",
	callback = function()
		vim.bo.tabstop = 4
		vim.bo.softtabstop = 4
		vim.bo.shiftwidth = 4
		vim.bo.expandtab = false
	end,
})
vim.api.nvim_create_autocmd("FileType", {
	pattern = "lua",
	callback = function()
		vim.bo.tabstop = 2
		vim.bo.softtabstop = 2
		vim.bo.shiftwidth = 2
		vim.bo.expandtab = true
	end,
})

vim.lsp.config("*", {
	root_markers = { ".git" },
	on_attach = function(_, bufnr)
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
vim.lsp.config("gopls", {
	cmd = { "gopls" },
	filetypes = { "go", "gomod", "gowork", "gotmpl" },
	root_dir = vim.fs.dirname(vim.fs.find({ "go.work", "go.mod", ".git" }, { upward = true })[1]),
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
})
vim.lsp.enable("gopls")
vim.lsp.config("lua_ls", {
	cmd = { "lua-language-server" },
	filetypes = { "lua" },
	on_init = function(client)
		if client.workspace_folders then
			local path = client.workspace_folders[1].name
			if
				path ~= vim.fn.stdpath("config")
				and (vim.uv.fs_stat(path .. "/.luarc.json") or vim.uv.fs_stat(path .. "/.luarc.jsonc"))
			then
				return
			end
		end
		client.config.settings.Lua = vim.tbl_deep_extend("force", client.config.settings.Lua, {
			runtime = { version = "LuaJIT" },
			workspace = {
				checkThirdParty = false,
				library = vim.list_extend(vim.api.nvim_get_runtime_file("lua", true), {
					"${3rd}/luv/library",
					"${3rd}/busted/library",
				}),
			},
		})
	end,
	settings = {
		Lua = {
			diagnostics = {
				unusedLocalExclude = { "_*" },
			},
		},
	},
})
vim.lsp.enable("lua_ls")
