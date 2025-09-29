vim.keymap.set("i", "<C-n>", "<CMD>call pum#map#insert_relative(+1)<CR>")
vim.keymap.set("i", "<C-p>", "<CMD>call pum#map#insert_relative(-1)<CR>")
vim.keymap.set("i", "<C-y>", "<CMD>call pum#map#confirm()<CR>")
vim.keymap.set("i", "<C-e>", "<CMD>call pum#map#cancel()<CR>")
vim.keymap.set("i", "<PageDown>", "<CMD>call pum#map#insert_relative_page(+1)<CR>")
vim.keymap.set("i", "<PageUp>", "<CMD>call pum#map#insert_relative_page(-1)<CR>")

vim.fn["ddc#custom#patch_global"]({
	ui = "pum",
	autoCompleteEvents = {
		"InsertEnter",
		"TextChangedI",
		"TextChangedP",
	},
	sources = { "lsp", "around" },
	sourceOptions = {
		_ = {
			ignoreCase = true,
			matchers = { "matcher_fuzzy" },
			sorters = { "sorter_fuzzy" },
		},
		around = {
			mark = "[ARD]",
		},
		lsp = {
			mark = "[LSP]",
			isVolatile = true,
			forceCompletionPattern = [[\.\w*|:\w*|->\w*]],
			dup = "keep",
			keywordPattern = [[\k+]],
			sorters = { "sorter_lsp_kind" },
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
