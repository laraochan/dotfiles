vim.g.mapleader = " "

      vim.g.loaded_netrw = 1
      vim.g.loaded_netrwPlugin = 1

	  	vim.cmd("filetype indent plugin on")
      vim.cmd("syntax on")
      
      vim.o.clipboard = "unnamedplus"
      vim.o.mouse = "a"
      vim.o.termguicolors = true
      vim.o.backup = false
      vim.o.writebackup = false
      vim.o.swapfile = false
      vim.o.undofile = false 
      vim.o.number = true
      vim.o.signcolumn = "yes"
      vim.o.wrap = false
      vim.o.tabstop = 4
      vim.o.shiftwidth = 4
      vim.o.softtabstop = 4
      vim.o.expandtab = true
      vim.o.smartindent = true
      vim.o.autoindent = true
      
      vim.o.winborder = "rounded"
      
      vim.keymap.set("n", "<Leader>bn", "<Cmd>bn<Cr>", { desc = "move buffer next" })
      vim.keymap.set("n", "<Leader>bp", "<Cmd>bp<Cr>", { desc = "move buffer prev" })
      vim.keymap.set("n", "<Leader>bd", "<Cmd>bd<Cr>", { desc = "delete buffer" })
      
      vim.diagnostic.config({
        virtual_text = true,
        signs = true,
        underline = true,
        update_in_insert = false,
        float = { border = "rounded" }
      })
      
      vim.keymap.set("n", "<Leader>dd", vim.diagnostic.open_float, { desc = "show diagnostic" })
      vim.keymap.set("n", "<Leader>dn", vim.diagnostic.goto_next, { desc = "goto next diagnostic" })
      vim.keymap.set("n", "<Leader>dp", vim.diagnostic.goto_prev, { desc = "goto prev diagnostic" })

      local tab_group = vim.api.nvim_create_augroup("TagSettings", { clear = true })
      vim.api.nvim_create_autocmd("FileType", {
        group = tab_group,
        pattern = { "lua", "javascript", "javascriptreact", "typescript", "typescriptreact", "yaml", "toml" },
        callback = function()
          vim.opt_local.expandtab = true
          vim.opt_local.tabstop = 2
          vim.opt_local.shiftwidth = 2
          vim.opt_local.softtabstop = 2
          vim.opt_local.autoindent = true
          vim.opt_local.smartindent = true
        end,
      })
      vim.api.nvim_create_autocmd("FileType", {
        group = tab_group,
        pattern = { "go" },
        callback = function()
          vim.opt_local.expandtab = false
          vim.opt_local.tabstop = 4
          vim.opt_local.shiftwidth = 4
          vim.opt_local.softtabstop = 4
          vim.opt_local.autoindent = true
          vim.opt_local.smartindent = true
        end,
      })