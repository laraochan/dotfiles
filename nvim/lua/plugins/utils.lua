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
      "gbprod/none-ls-ecs.nvim",
    },
    config = function()
      local null_ls = require("null-ls")

      null_ls.setup({
        sources = {
          null_ls.builtins.formatting.stylua,
          require("none-ls.formatting.eslint_d"),
          require("none-ls.diagnostics.eslint_d"),
          require("none-ls.code_actions.eslint_d"),
          null_ls.builtins.formatting.prettierd.with({
            filetypes = {
              "json",
              "yaml",
            },
          }),
          null_ls.builtins.diagnostics.phpstan.with({
            -- timeoutはdefaultで5sで設定されていてremote環境ではtimeoutしてしまっていたため10sに伸ばした
            timeout = 10000,
            command = "dev-script/phpstan",
            args = {
              "analyze",
              "--error-format",
              "json",
              "--configuration",
              "phpstan.neon.dist",
              "--no-progress",
              "$FILENAME",
            },
            condition = function()
              local cwd = vim.loop.cwd()
              return cwd:match("/pixiv$")
            end,
          }),
          require("none-ls-ecs.formatting").with({
            command = "dev-script/ecs",
            condition = function()
              local cwd = vim.loop.cwd()
              return cwd:match("/pixiv$")
            end,
          }),
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
      "nvimtools/none-ls.nvim",
    },
    config = function()
      vim.lsp.config("*", {
        capabilities = require("cmp_nvim_lsp").default_capabilities(),
      })
      vim.lsp.enable("ts_ls")
      vim.lsp.enable("gopls")
      vim.lsp.enable("rust_analyzer")
      vim.lsp.enable("lua_ls")
      vim.lsp.enable("intelephense")
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
        symbol_in_winbar = {
          enable = false,
        },
        lightbulb = {
          enable = false,
        },
      })

      vim.keymap.set("n", "K", "<CMD>Lspsaga hover_doc<CR>")
      vim.keymap.set("n", "gr", "<CMD>Lspsaga finder ref<CR>")
      vim.keymap.set("n", "gp", "<CMD>Lspsaga finder imp<CR>")
      vim.keymap.set("n", "gd", "<CMD>Lspsaga peek_definition<CR>")
      vim.keymap.set("n", "ga", "<CMD>Lspsaga code_action<CR>")
      vim.keymap.set("n", "gn", "<CMD>Lspsaga rename<CR>")
      vim.keymap.set("n", "ge", "<CMD>Lspsaga show_line_diagnostics<CR>")
      vim.keymap.set("n", "[e", "<CMD>Lspsaga diagnostic_jump_next<CR>")
      vim.keymap.set("n", "]e", "<CMD>Lspsaga diagnostic_jump_prev<CR>")

      vim.keymap.set("n", "<leader>tt", "<CMD>Lspsaga term_toggle<CR>")
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
    "windwp/nvim-ts-autotag",
    config = function()
      require("nvim-ts-autotag").setup()
    end,
  },
  {
    "folke/flash.nvim",
    event = "VeryLazy",
    -- stylua: ignore
    keys = {
      { "<leader>ss", mode = { "n", "x", "o" }, function() require("flash").jump() end,       desc = "Flash" },
      { "<leader>s[", mode = { "n", "x", "o" }, function() require("flash").treesitter() end, desc = "Flash Treesitter" },
    },
  },
}
