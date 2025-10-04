inoremap <C-n>   <Cmd>call pum#map#insert_relative(+1)<CR>
inoremap <C-p>   <Cmd>call pum#map#insert_relative(-1)<CR>
inoremap <C-y>   <Cmd>call pum#map#confirm()<CR>
inoremap <C-e>   <Cmd>call pum#map#cancel()<CR>
inoremap <PageDown> <Cmd>call pum#map#insert_relative_page(+1)<CR>
inoremap <PageUp>   <Cmd>call pum#map#insert_relative_page(-1)<CR>

call ddc#custom#patch_global(#{
	\ ui: 'pum',
  \ autoCompleteEvents: [
  \   'InsertEnter',
  \   'TextChangedI',
  \   'TextChangedP',
  \   'CmdlineChanged',
  \ ],
  \ cmdlineSources: {
  \   ':': [
  \     'cmdline',
  \     'cmdline_history',
  \     'around',
  \   ],
  \ },
	\ sources: [ 'around', 'lsp' ],
	\ sourceOptions: #{
  \   _: #{
	\		  ignoreCase: v:true,
	\		  matchers: [ 'matcher_fuzzy' ],
	\		  sorters: [ 'sorter_fuzzy', 'sorter_lsp_kind' ],
  \     converters: [ 'converter_fuzzy' ],
	\ 	},
	\	  around: #{
	\		  mark: '[ALD]',
	\	  },
	\	  lsp: #{
	\		  mark: '[LSP]',
	\		  isVolatile: v:true,
	\		  forceCompletionPattern: '\.\w*|:\w*|->\w*',
	\	  },
  \   cmdline: #{
  \     mark: '[CMD]'
  \   },
  \   cmdline_history: #{
  \     mark: '[HIS]',
  \   },
	\ },
	\ sourceParams: #{
	\	  lsp: #{
	\		  lspEngine: 'vim-lsp',
	\		  enableResolveItem: v:true,
	\		  enableAdditionalTextEdit: v:true,
	\	  },
  \ },
  \ filterParams: #{
  \   converter_fuzzy: #{
  \     hlGroup: 'SpellBad',
  \   },
  \ }})
call ddc#enable()


nnoremap : <Cmd>call CommandlinePre()<CR>:

function! CommandlinePre() abort
  cnoremap <C-n> <Cmd>call pum#map#insert_relative(+1)<CR>
  cnoremap <C-p> <Cmd>call pum#map#insert_relative(-1)<CR>
  cnoremap <C-y> <Cmd>call pum#map#confirm()<CR>
  cnoremap <C-e> <Cmd>call pum#map#cancel()<CR>

  autocmd User DDCCmdlineLeave ++once call CommandlinePost()

  call ddc#enable_cmdline_completion()
endfunction
function! CommandlinePost() abort
  silent! cunmap <C-n>
  silent! cunmap <C-p>
  silent! cunmap <C-y>
  silent! cunmap <C-e>
endfunction
