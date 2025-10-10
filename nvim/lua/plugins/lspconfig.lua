return {
  {
    "neovim/nvim-lspconfig",
    config = function()
      vim.lsp.enable("ts_ls")
      vim.lsp.enable("gopls")
      vim.lsp.enable("intelephense")
    end,
  }
}
