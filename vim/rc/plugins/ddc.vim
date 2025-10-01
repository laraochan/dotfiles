inoremap <C-n>   <Cmd>call pum#map#insert_relative(+1)<CR>
inoremap <C-p>   <Cmd>call pum#map#insert_relative(-1)<CR>
inoremap <C-y>   <Cmd>call pum#map#confirm()<CR>
inoremap <C-e>   <Cmd>call pum#map#cancel()<CR>
inoremap <PageDown> <Cmd>call pum#map#insert_relative_page(+1)<CR>
inoremap <PageUp>   <Cmd>call pum#map#insert_relative_page(-1)<CR>

call ddc#custom#patch_global(#{
	\ ui: 'pum',
	\ sources: ['around', 'lsp'],
	\ sourceOptions: #{
        \       _: #{
	\		ignoreCase: v:true,
	\		matchers: ['matcher_fuzzy'],
	\		sorters: ['sorter_fuzzy'],
	\ 	},
	\	around: #{
	\		mark: '[ALD]',
	\	},
	\	lsp: #{
	\		mark: '[LSP]',
	\		isVolatile: v:true,
	\		forceCompletionPattern: '\.\w*|:\w*|->\w*',
	\	},
	\ },
	\ sourceParams: #{
	\	lsp: #{
	\		lspEngine: 'vim-lsp',
	\		enableResolveItem: v:true,
	\		enableAdditionalTextEdit: v:true,
	\	},
	\ }})
call ddc#enable()

