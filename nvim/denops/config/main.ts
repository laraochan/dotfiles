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
          vim.keymap.set("i", "<c-n>", "<cmd>call pum#map#insert_relative(+1)<cr>")
          vim.keymap.set("i", "<c-p>", "<cmd>call pum#map#insert_relative(-1)<cr>")
          vim.keymap.set("i", "<c-y>", "<cmd>call pum#map#confirm()<cr>")
          vim.keymap.set("i", "<c-e>", "<cmd>call pum#map#cancel()<cr>")

          vim.fn["ddc#custom#patch_global"]({
            ui = "pum",
            autoCompleteDelay = 100,
            sources = { "lsp", "around" },
            sourceOptions = {
              _ = {
                matchers = { "matcher_fuzzy" },
                sorters = { "sorter_fuzzy" },
                converters = { "converter_fuzzy" },
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
            filterParams = {
              converter_fuzzy = {
                hlGroup = "SpellBad",
              },
            },
          })
          vim.fn["ddc#enable"]()
          vim.fn["popup_preview#enable"]()
          vim.fn["signature_help#enable"]()
        EOB
      `,
    },
  });
  await dvpm.add({ url: "Shougo/pum.vim" })
  await dvpm.add({ url: "Shougo/ddc-ui-pum" })
  await dvpm.add({ url: "tani/ddc-fuzzy" })
  await dvpm.add({ url: "Shougo/ddc-source-around" })
  await dvpm.add({ url: "Shougo/ddc-source-lsp" })
  await dvpm.add({ url: "matsui54/denops-popup-preview.vim" })
  await dvpm.add({ url: "matsui54/denops-signature_help" })

	await dvpm.end();

	console.log("Load completed!");
}
