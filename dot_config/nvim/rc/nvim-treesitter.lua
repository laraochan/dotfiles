require("nvim-treesitter.configs").setup({
  ensure_installed = { "lua", "vim", "vimdoc", "typescript", "javascript", "tsx", "toml", "yaml" },
  highlight = {
    enable = true
  }
})
