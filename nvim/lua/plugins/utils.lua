return {
  {
    "numToStr/Comment.nvim",
    config = function()
      require("Comment").setup()
    end,
  },
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    config = true,
  },
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
  },
  {
    "nvim-lua/plenary.nvim",
  },
  {
    "nvim-telescope/telescope.nvim",
    dependencies = {
      "nvim-telescope/telescope-file-browser.nvim",
    },
    config = function()
      require("telescope").setup({
        defaults = {
          border = false,
        },
      })

      local builtin = require("telescope.builtin")
      local extensions = require("telescope").extensions
      vim.keymap.set("n", "<leader>ff", builtin.find_files, { desc = "Telescope find files" })
      vim.keymap.set("n", "<leader>fg", builtin.live_grep, { desc = "Telescope live grep" })
      vim.keymap.set("n", "<leader>fb", builtin.buffers, { desc = "Telescope buffers" })
      vim.keymap.set("n", "<leader>fh", builtin.help_tags, { desc = "Telescope help tags" })
      vim.keymap.set("n", "<leader>fe", function()
        extensions.file_browser.file_browser()
      end, { desc = "Telescppe file_browser" })

      -- 起動時にフォルダなら Telescope file_browser を開く
      vim.api.nvim_create_autocmd("VimEnter", {
        callback = function(data)
          local is_dir = vim.fn.isdirectory(data.file) == 1
          if is_dir then
            -- ディレクトリをカレントに設定
            vim.cmd.cd(data.file)
            -- Telescope file_browser を開く
            require("telescope").extensions.file_browser.file_browser({
              path = data.file,
              cwd = data.file,
            })
          end
        end,
      })
    end,
  },
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-cmdline",
    },
    config = function()
      local cmp = require("cmp")

      cmp.setup({
        mapping = cmp.mapping.preset.insert({
          ["<C-b>"] = cmp.mapping.scroll_docs(-4),
          ["<C-f>"] = cmp.mapping.scroll_docs(4),
          ["<C-Space>"] = cmp.mapping.complete(),
          ["<C-e>"] = cmp.mapping.abort(),
          ["<CR>"] = cmp.mapping.confirm({ select = true }),
        }),
        sources = cmp.config.sources({
          { name = "nvim_lsp" },
        }, {
          { name = "buffer" },
        }),
      })

      cmp.setup.cmdline({ "/", "?" }, {
        mapping = cmp.mapping.preset.cmdline(),
        sources = {
          { name = "buffer" },
        },
      })

      cmp.setup.cmdline(":", {
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources({
          { name = "path" },
        }, {
          { name = "cmdline" },
        }),
      })
    end,
  },
  {
    "nvimtools/none-ls.nvim",
    dependencies = {
      "nvimtools/none-ls-extras.nvim",
    },
    config = function()
      local null_ls = require("null-ls")
      local augroup = vim.api.nvim_create_augroup("LspFormatting", {})

      null_ls.setup({
        sources = {
          null_ls.builtins.formatting.stylua,
          -- require("none-ls.diagnostics.eslint_d"),
        },
        on_attach = function(client, bufnr)
          if client.supports_method("textDocument/formatting") then
            vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
            vim.api.nvim_create_autocmd("BufWritePre", {
              group = augroup,
              buffer = bufnr,
              callback = function()
                vim.lsp.buf.format({ async = false })
              end,
            })
          end
        end,
      })
    end,
  },
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      "hrsh7th/cmp-nvim-lsp",
    },
    config = function()
      local capabilities = require("cmp_nvim_lsp").default_capabilities()
      vim.lsp.config("*", {
        capabilities = capabilities,
      })
      vim.lsp.enable("ts_ls")
      vim.lsp.enable("gopls")
      vim.lsp.enable("rust_analyzer")
    end,
  },
  {
    "nvimdev/lspsaga.nvim",
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
      "nvim-tree/nvim-web-devicons",
    },
    config = function()
      require("lspsaga").setup({
        definition = {
          keys = {
            edit = "o",
          },
        },
        lightbulb = {
          enable = false,
        },
      })

      vim.keymap.set("n", "K", "<cmd>Lspsaga hover_doc<CR>")
      vim.keymap.set("n", "gr", "<cmd>Lspsaga finder ref<CR>")
      vim.keymap.set("n", "gp", "<cmd>Lspsaga finder imp<CR>")
      vim.keymap.set("n", "gd", "<cmd>Lspsaga peek_definition<CR>")
      vim.keymap.set("n", "ga", "<cmd>Lspsaga code_action<CR>")
      vim.keymap.set("n", "gn", "<cmd>Lspsaga rename<CR>")
      vim.keymap.set("n", "ge", "<cmd>Lspsaga show_line_diagnostics<CR>")
      vim.keymap.set("n", "[e", "<cmd>Lspsaga diagnostic_jump_next<CR>")
      vim.keymap.set("n", "]e", "<cmd>Lspsaga diagnostic_jump_prev<CR>")
    end,
  },
  {
    "mason-org/mason.nvim",
    dependencies = {
      "jay-babu/mason-null-ls.nvim",
    },
    config = function()
      require("mason").setup()
      require("mason-null-ls").setup()
    end,
  },
  {
    "akinsho/toggleterm.nvim",
    config = function()
      require("toggleterm").setup()
      vim.keymap.set("n", "<leader>tt", "<CMD>ToggleTerm<CR>")
    end,
  },
  {
    "windwp/nvim-ts-autotag",
    config = function()
      require("nvim-ts-autotag").setup()
    end,
  },
}
