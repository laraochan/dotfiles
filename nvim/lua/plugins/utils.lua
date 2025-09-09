return {
  {
    "AlexvZyl/nordic.nvim",
    lazy = false,
    config = function()
      require("nordic").load()
    end,
  },
  {
    "nvim-treesitter/nvim-treesitter",
    lazy = false,
    build = ":TSUpdate",
    config = function()
      local treesitter = require("nvim-treesitter.configs")
      treesitter.setup({
        ensure_installed = { "vim", "vimdoc", "lua" },
        highlight = {
          enable = true,
        },
      })
    end,
  },
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    config = true,
  },
  {
    "numToStr/Comment.nvim",
    config = function()
      require("Comment").setup()
    end,
  },
  {
    "saghen/blink.cmp",
    opts = {
      keymap = { preset = "enter" },
      appearance = {
        nerd_font_variant = "mono",
      },
      completion = { documentation = { auto_show = true } },
      sources = {
        default = { "lsp", "path", "buffer" },
      },
      fuzzy = { implementation = "lua" },
    },
    opts_extend = { "sources.default" },
  },
  {
    "neovim/nvim-lspconfig",
    dependencies = { "saghen/blink.cmp" },
    config = function()
      local servers = {
        lua_ls = {},
        ts_ls = {},
        gopls = {},
        intelephense = {},
      }
      for server, config in pairs(servers) do
        config.capabilities = require("blink.cmp").get_lsp_capabilities(config.capabilities)
        config.on_attach = function(client, bufnr)
          local opts = { noremap = true, silent = true, buffer = bufnr }
          local border = {
            { "╭", "DiagnosticBorder" },
            { "─", "DiagnosticBorder" },
            { "╮", "DiagnosticBorder" },
            { "│", "DiagnosticBorder" },
            { "╯", "DiagnosticBorder" },
            { "─", "DiagnosticBorder" },
            { "╰", "DiagnosticBorder" },
            { "│", "DiagnosticBorder" },
          }

          vim.keymap.set("n", "<leader>k", vim.lsp.buf.hover, opts)
          vim.keymap.set("n", "<leader>gd", vim.lsp.buf.definition, opts)
          vim.keymap.set("n", "<leader>gD", vim.lsp.buf.type_definition, opts)
          vim.keymap.set("n", "<leader>gi", vim.lsp.buf.implementation, opts)
          vim.keymap.set("n", "<leader>gr", vim.lsp.buf.references, opts)
          vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, opts)
          vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, opts)
          vim.keymap.set("n", "<leader>e", function()
            vim.diagnostic.open_float(nil, { border = border })
          end, opts)
          vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, opts)
          vim.keymap.set("n", "d]", vim.diagnostic.goto_next, opts)
          vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, opts)
          vim.keymap.set("n", "<leader>dh", vim.diagnostic.hide, opts)
          vim.keymap.set("n", "<leader>ds", vim.diagnostic.show, opts)
        end
        vim.lsp.config(server, config)
        vim.lsp.enable(server)
      end
    end,
  },
  {
    "lewis6991/gitsigns.nvim",
    config = function()
      require("gitsigns").setup()
    end,
  },
  {
    "nvim-tree/nvim-tree.lua",
    config = function()
      require("nvim-tree").setup()

      -- autocmdでnvim-treeだけが残った時に自動終了
      vim.api.nvim_create_autocmd("QuitPre", {
        callback = function()
          local tree_wins = {}
          local floating_wins = {}
          local wins = vim.api.nvim_list_wins()
          for _, w in ipairs(wins) do
            local bufname = vim.api.nvim_buf_get_name(vim.api.nvim_win_get_buf(w))
            if bufname:match("NvimTree_") ~= nil then
              table.insert(tree_wins, w)
            end
            if vim.api.nvim_win_get_config(w).relative ~= "" then
              table.insert(floating_wins, w)
            end
          end
          if 1 == #wins - #floating_wins - #tree_wins then
            -- nvim-treeだけが残っている場合は強制終了
            vim.cmd("qall!")
          end
        end,
      })
    end,
  },
  {
    "ibhagwan/fzf-lua",
    dependencies = { "nvim-tree/nvim-web-devicons" },
  },
  {
    "romgrk/barbar.nvim",
    dependencies = {
      "lewis6991/gitsigns.nvim",
      "nvim-tree/nvim-web-devicons",
    },
    config = function()
      require("barbar").setup({
        animation = false,
        auto_hide = true,
      })
    end,
  },
  {
    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require("lualine").setup()
    end,
  },
  {
    "nvimtools/none-ls.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
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
    "windwp/nvim-ts-autotag",
    config = function()
      require("nvim-ts-autotag").setup()
    end,
  },
  {
    "akinsho/toggleterm.nvim",
    config = function()
      require("toggleterm").setup({
        size = 30,
        open_mapping = [[<leader>tt]],
      })
    end,
  },
}
