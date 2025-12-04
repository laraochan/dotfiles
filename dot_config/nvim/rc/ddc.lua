vim.lsp.config('*', {
  capabilities = require("ddc_source_lsp").make_client_capabilities(),
})

vim.keymap.set("i", "<C-n>", "<Cmd>call pum#map#insert_relative(+1)<Cr>")
vim.keymap.set("i", "<C-p>", "<Cmd>call pum#map#insert_relative(-1)<Cr>")
vim.keymap.set("i", "<C-y>", "<Cmd>call pum#map#confirm()<Cr>")
vim.keymap.set("i", "<C-e>", "<Cmd>call pum#map#cancel()<Cr>")
vim.keymap.set("i", "<PageDown>", "<Cmd>call pum#map#insert_relative_page(+1)<Cr>")
vim.keymap.set("i", "<PageUp>", "<Cmd>call pum#map#insert_relative_page(-1)<Cr>")

vim.fn["ddc#custom#patch_global"]({
  ui = "pum",
  sources = { "lsp" },
  autoCompleteDelay = 200,
  sourceOptions = {
    _ = {
      matchers = { "matcher_fuzzy" },
      sorters = { "sorter_fuzzy" },
    },
    lsp = {
      mark = "[LSP]",
      dup = "keep",
      isVolatile = true,
      ignoreCase = true,
      forceCompletionPattern = [[\.\w*|:\w*|->\w*]],
    },
  },
  sourceParams = {
    lspEngine = "nvim-lsp",
    enableResolveItem = true,
    enableAdditionalTextEdit = true,
  },
})
vim.fn["ddc#enable"]()
vim.fn["popup_preview#enable"]()
