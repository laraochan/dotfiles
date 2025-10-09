return {
  {
    "nvimdev/lspsaga.nvim",
    dependepcies = {
      "nvim-treesitter/nvim-treesitter",
      "nvim-tree/nvim-web-devicons",
    },
    config = function()
      require("lspsaga").setup({
        lightbulb = {
          enable = false,
        }
      })
      vim.keymap.set("n", "K", "<Cmd>Lspsaga hover_doc<CR>")
      vim.keymap.set("n", "gd", "<Cmd>Lspsaga peek_definition<CR>")
      vim.keymap.set("n", "gD", "<Cmd>Lspsaga peek_type_definition<CR>")
      vim.keymap.set("n", "gR", "<Cmd>Lspsaga finder ref<CR>")
      vim.keymap.set("n", "gI", "<Cmd>Lspsaga finder imp<CR>")
      vim.keymap.set("n", "]e", "<Cmd>Lspsaga diagnostic_jump_next<CR>")
      vim.keymap.set("n", "[e", "<Cmd>Lspsaga diagnostic_jump_prev<CR>")
      vim.keymap.set("n", "ge", "<Cmd>Lspsaga show_cursor_diagnostic<CR>")
      vim.keymap.set("n", "ca", "<Cmd>Lspsaga code_action<CR>")
      vim.keymap.set("n", "gr", "<Cmd>Lspsaga rename<CR>")
    end,
  },
}
