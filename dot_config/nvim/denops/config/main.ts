import type { Denops, Entrypoint } from "@denops/std";
import * as fn from "@denops/std/function";
import * as mapping from "@denops/std/mapping";
import * as vars from "@denops/std/variable";
import { ensure, is } from "@core/unknownutil";
import { execute } from "@denops/std/helper";

import { Dvpm } from "@yukimemi/dvpm";

export const main: Entrypoint = async (denops: Denops) => {
  const base_path = (await fn.has(denops, "nvim")) ? "~/.cache/nvim/dvpm" : "~/.cache/vim/dvpm";
  const base = ensure(await fn.expand(denops, base_path), is.String);
  const cache_path = (await fn.has(denops, "nvim"))
    ? "~/.config/nvim/plugin/dvpm_plugin_cache.vim"
    : "~/.config/vim/plugin/dvpm_plugin_cache.vim";
  const cache = ensure(await fn.expand(denops, cache_path), is.String);

  const dvpm = await Dvpm.begin(denops, { base, cache });

  await dvpm.cache({
	  script: `
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
	  `,
	  path: "~/.config/nvim/plugin/dvpm_cache.lua"
  })

  await dvpm.add({
    url: "oahlen/iceberg.nvim",
    cache: {
      beforeFile: "~/.config/nvim/rc/iceberg.lua"
    }
  })

  await dvpm.add({
    url: "Shougo/ddc.vim",
    dependencies: [
      "Shougo/ddc-ui-pum",
      "Shougo/pum.vim",
      "Shougo/ddc-source-lsp",
      "neovim/nvim-lspconfig",
      "tani/ddc-fuzzy",
      "matsui54/denops-popup-preview.vim",
    ],
    afterFile: "~/.config/nvim/rc/ddc.lua",
  })
  await dvpm.add({ url: "Shougo/ddc-ui-pum" })
  await dvpm.add({ url: "Shougo/pum.vim" })
  await dvpm.add({ url: "Shougo/ddc-source-lsp" })
  await dvpm.add({
    url: "neovim/nvim-lspconfig",
    afterFile: "~/.config/nvim/rc/nvim-lspconfig.lua",
  })
  await dvpm.add({ url: "tani/ddc-fuzzy" })
  await dvpm.add({
    url: "matsui54/denops-popup-preview.vim",
  })

  await dvpm.add({
    url: "nvim-treesitter/nvim-treesitter",
    build: async ({ denops, info }) => {
      if (!info.isUpdate || !info.isLoad) {
        return;
      }
      await execute(denops, ":TSUpdate")
    },
    afterFile: "~/.config/nvim/rc/nvim-treesitter.lua"
  })

  await dvpm.add({
    url: "lewis6991/gitsigns.nvim",
    afterFile: "~/.config/nvim/rc/gitsigns.lua"
  })

  // TODO: write ddu config
  await dvpm.add({
    url: "Shougo/ddu.vim",
    dependencies: [
      "Shougo/ddu-ui-filer",
    ],
    afterFile: "~/.config/nvim/rc/ddu.lua"
  })
  await dvpm.add({
    url: "Shougo/ddu-ui-filer",
  })
  await dvpm.add({
    url: "Shougo/ddu-ui-ff"
  })
  await dvpm.add({
    url: "Shougo/ddu-source-file",
  })
  await dvpm.add({
    url: "Shougo/ddu-kind-file",
  })
  await dvpm.add({
    url: "Shougo/ddu-column-filename",
  })

  await dvpm.add({
    url: "yukimemi/autocursor.vim",
    afterFile: "~/.config/nvim/rc/autocursor.lua"
  })
  
  await dvpm.add({
    url: "stevearc/oil.nvim",
    dependencies: ["nvim-mini/mini.icons"],
    afterFile: "~/.config/nvim/rc/oil.lua",
  })

  await dvpm.add({
    url: "nvim-mini/mini.icons",
    afterFile: "~/.config/nvim/rc/mini-icons.lua",
  })

  await dvpm.add({
    url: "vim-fall/fall.vim",
    afterFile: "~/.config/nvim/rc/fall.lua",
  })

  await dvpm.add({
    url: "kdheepak/lazygit.nvim",
    dependencies: ["nvim-lua/plenary.nvim"],
    afterFile: "~/.config/nvim/rc/lazygit.lua",
  })
  
  await dvpm.add({
    url: "nvim-lua/plenary.nvim",
  })

  await dvpm.add({
    url: "akinsho/toggleterm.nvim",
    afterFile: "~/.config/nvim/rc/toggleterm.lua",
  })

  await dvpm.end();
    
  console.log("dvpm load completed!");
};
