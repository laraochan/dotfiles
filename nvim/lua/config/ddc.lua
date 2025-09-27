vim.keymap.set('i', '<C-n>', '<CMD>call pum#map#insert_relative(+1)<CR>')
vim.keymap.set('i', '<C-p>', '<CMD>call pum#map#insert_relative(-1)<CR>')
vim.keymap.set('i', '<C-y>', '<CMD>call pum#map#confirm()<CR>')
vim.keymap.set('i', '<C-e>', '<CMD>call pum#map#cancel()<CR>')
vim.keymap.set('i', '<PageDown>', '<CMD>call pum#map#insert_relative_page(+1)<CR>')
vim.keymap.set('i', '<PageUp>', '<CMD>call pum#map#insert_relative_page(-1)<CR>')

vim.lsp.config('*', {
  capabilities = require("ddc_source_lsp").make_client_capabilities(),
})

vim.fn["ddc#custom#patch_global"]({
	ui = "pum",
	sources = { "lsp", "around" },
	sourceOptions = {
		_ = {
			matchers = { "matcher_head" },
			sorters = { "sorter_rank" },
			minAutoCompleteLength = 1,
		},
		around = {
			mark = "[A]",
		},
		lsp = {
			mark = "[L]",
			isVolatile = true,
			forceCompletionPattern = [[\.\w*|:\w*|->\w*]],
		},
	},
	sourceParams = {
		lsp = {
			enableResolveItem = true,
			enableAdditionalTextEdit = true,
		},
	},
})

vim.fn["ddc#enable"]()
