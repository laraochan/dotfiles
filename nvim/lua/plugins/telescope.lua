return {
  {
    "nvim-telescope/telescope.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope-file-browser.nvim",
    },
    config = function()
      local telescope = require("telescope")
      local actions = require("telescope.actions")

      telescope.setup({
        pickers = {
          buffers = {
            mappings = {
              n = {
                ["dd"] = actions.delete_buffer + actions.move_to_top,
              }
            }
          },
        },
        extensions = {
          file_browser = {
            hijack_netrw = true,
          }
        }
      })
      telescope.load_extension("file_browser")


      local builtin = require("telescope.builtin")
      vim.keymap.set("n", "<leader>ff", builtin.find_files, { desc = "Telescope find files" })
      vim.keymap.set("n", "<leader>fg", builtin.live_grep, { desc = "Telescope live grep" })
      vim.keymap.set("n", "<leader>fb", builtin.buffers, { desc = "Telescope buffers" })
      vim.keymap.set("n", "<leader>fh", builtin.help_tags, { desc = "Telescope help tags" })

      vim.keymap.set("n", "<leader>fe", function()
        telescope.extensions.file_browser.file_browser()
      end)
    end,
  },
}
