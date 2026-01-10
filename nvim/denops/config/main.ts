import type { Denops, Entrypoint } from "@denops/std";
import * as fn from "@denops/std/function";
import * as mapping from "@denops/std/mapping";
import * as vars from "@denops/std/variable";
import { execute } from "@denops/std/helper";

import { Dvpm } from "@yukimemi/dvpm";

export const main: Entrypoint = async (denops: Denops) => {
	const basePath = (await fn.has(denops, "nvim")) ? "~/.cache/nvim/dvpm" : "~/.cache/vim/dvpm";
	const base = (await fn.expand(denops, basePath)) as string;
	const cachePath = (await fn.has(denops, "nvim"))
		? "~/.config/nvim/plugin/dvpm_plugin_cache.vim"
		: "~/.config/vim/plugin/dvpm_plugin_cache.vim";
	const cache = (await fn.expand(denops, cachePath)) as string;

	const dvpm = await Dvpm.begin(denops, { base, cache });

	await dvpm.cache({
		script: `
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
		`,
		path: "~/.config/nvim/plugin/dvpm_cache.lua",
	});

	await dvpm.add({
		url: "folke/tokyonight.nvim",
		cache: {
			after: `
				lua << EOB
					vim.cmd.colorscheme("tokyonight-storm")
				EOB
			`,
		},
	});

	await dvpm.add({
		url: "nvim-treesitter/nvim-treesitter",
    enabled: async ({ denops }) => {
      const out = await new Deno.Command("hostname", { stdout: "piped" }).output();
      const hostname = new TextDecoder().decode(out.stdout).trim();
      return !/^z02[0-2]$/.test(hostname);
    },
		build: async ({ denops, info }) => {
			if (!(info.isInstalled || info.isUpdated) || !info.isLoaded) {
				return;
			}
			await denops.cmd("TSUpdate")
		},
		cache: {
			after: `
				lua << EOB
					require("nvim-treesitter").install({ "typescript", "tsx", "javascript", "jsdoc", "json", "toml", "yaml", "lua", "vim", "vimdoc" })
					vim.api.nvim_create_autocmd("FileType", {
						pattern = { "typescript", "typescriptreact", "javascript", "javascriptreact", "jsdoc", "json", "toml", "yaml", "lua", "vim", "vimdoc" },
						callback = function() vim.treesitter.start() end,
					})
				EOB
			`,
		},
	});

  await dvpm.add({
    url: "neovim/nvim-lspconfig",
    cache: {
      after: `
        lua << EOB
          vim.lsp.enable("vtsls")
        EOB
      `,
    },
  });

  await dvpm.add({
    url: "Shougo/ddc.vim",
    dependencies: [
      "Shougo/pum.vim",
      "Shougo/ddc-ui-pum",
      "tani/ddc-fuzzy",
      "Shougo/ddc-source-around",
      "Shougo/ddc-source-lsp",
      "matsui54/denops-popup-preview.vim",
      "matsui54/denops-signature_help",
    ],
    cache: {
      after: `
        lua << EOB
          vim.fn["ddc#custom#patch_global"]({
            ui = "native",
            autoCompleteDelay = 100,
            sources = { "lsp", "around" },
            sourceOptions = {
              _ = {
                matchers = { "matcher_fuzzy" },
                sorters = { "sorter_fuzzy" },
              },
              around = {
                mark = "[AROUND]",
              },
              lsp = {
                minAutoCompleteLength = 1,
                mark = "[LSP]",
                ignoreCase = true,
                dup = "keep",
                isVolatile = true,
                forceCompletionPattern = [[\.\w*|:\w*|->\w*]],
              },
            },
            sourceParams = {
              lsp = {
                lspEngine = "nvim-lsp",
                enableResolveItem = true,
                enableAdditionalTextEdit = true,
              },
            },
          })
          vim.fn["ddc#enable"]()
        EOB
      `,
    },
  });
  await dvpm.add({ url: "Shougo/ddc-ui-native" });
  await dvpm.add({ url: "tani/ddc-fuzzy" });
  await dvpm.add({ url: "Shougo/ddc-source-lsp" });
  await dvpm.add({ url: "Shougo/ddc-source-around" })

  await dvpm.add({
    url: "Shougo/ddu.vim",
    cache: {
      after: `
        lua << EOB
          vim.fn["ddu#custom#patch_local"]("filer", {
            ui = "filer",
            sources = {{ name = "file" }},
            sourceOptions = {
              file = {
                ignoreCase = true,
                columns = { "icon_filename" },
                matchers = { "matcher_substring" },
              },
            },
            uiParams = {
              filer = {
                split = "no",
                sort = "filename",
                sortTreesFirst = true,
              },
            },
            columnParams = {
              icon_filename = {
                defaultIcon = { icon = "" },
              },
            },
          })
          vim.api.nvim_create_autocmd("FileType", {
            pattern = "ddu-filer",
            callback = function()
              vim.keymap.set("n", "<CR>", function()
                local item = vim.fn["ddu#ui#get_item"]()
                if item and item.isTree then
                  vim.fn["ddu#ui#do_action"]("itemAction", { name = "narrow" })
                else
                  vim.fn["ddu#ui#do_action"]("itemAction", { name = "open" })
                end
              end, {
                buffer = true,
                silent = true,
              })
              vim.keymap.set("n", "q", function() vim.fn["ddu#ui#do_action"]("itemAction", { name = "quit" }) end, { buffer = true, silent = true })
              vim.keymap.set("n", "n", function() vim.fn["ddu#ui#do_action"]("itemAction", { name = "newFile" }) end, { buffer = true, silent = true })
              vim.keymap.set("n", "d", function() vim.fn["ddu#ui#do_action"]("itemAction", { name = "delete" }) end, { buffer = true, silent = true })
              vim.keymap.set("n", "c", function() vim.fn["ddu#ui#do_action"]("itemAction", { name = "copy" }) end, { buffer = true, silent = true })
              vim.keymap.set("n", "p", function() vim.fn["ddu#ui#do_action"]("itemAction", { name = "paste" }) end, { buffer = true, silent = true })
              vim.keymap.set("n", "r", function() vim.fn["ddu#ui#do_action"]("itemAction", { name = "rename" }) end, { buffer = true, silent = true })
              vim.keymap.set("n", "m", function() vim.fn["ddu#ui#do_action"]("itemAction", { name = "move" }) end, { buffer = true, silent = true })
              vim.keymap.set("n", "f", function() vim.fn["ddu#ui#do_action"]("itemAction", { name = "openFilterWindow" }) end, { buffer = true, silent = true })
              vim.keymap.set("n", "v", function() vim.fn["ddu#ui#do_action"]("itemAction", { name = "togglePreview" }) end, { buffer = true, silent = true })
              vim.keymap.set("n", "..", function() vim.fn["ddu#ui#do_action"]("itemAction", { name = "narrow", params = { path = ".." } }) end, { buffer = true, silent = true })
            end,
          })
          vim.keymap.set("n", "<leader>e", function() vim.fn["ddu#start"]({ name = "filer" }) end, { silent = true })
        EOB
      `,
    },
  });
  await dvpm.add({ url: "Shougo/ddu-ui-filer" })
  await dvpm.add({ url: "Shougo/ddu-source-file" });
  await dvpm.add({ url: "Shougo/ddu-kind-file" });
  await dvpm.add({ url: "ryota2357/ddu-column-icon_filename" });
  await dvpm.add({ url: "Shougo/ddu-filter-matcher_substring" })

	await dvpm.end();

	console.log("Load completed!");
}
