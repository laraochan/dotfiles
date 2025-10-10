return {
  {
    "neovim/nvim-lspconfig",
    config = function()
      vim.lsp.enable("vtsls")
      vim.lsp.enable("gopls")
    end,
  }
}
