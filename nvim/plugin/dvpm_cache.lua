vim.o.expandtab = true
			vim.o.tabstop = 2
			vim.o.shiftwidth = 2
			vim.o.softtabstop = 2

			vim.o.autoindent = true
			vim.o.smartindent = true
			vim.o.smarttab = true
			vim.o.shiftround = true

			vim.o.breakindent = true

      vim.o.number = true
      vim.o.signcolumn = "yes"

      vim.api.nvim_create_autocmd("FileType", {
        pattern = { "go" },
        callback = function()
          vim.bo.expandtab = false
          vim.bo.tabstop = 4
          vim.bo.shiftwidth = 4
        end,
      })