call ddu#custom#patch_local('filer', #{
	\ ui: 'filer',
	\ sources: [#{ name: 'file', params: {} }],
	\ sourceOptions: #{
	\ 	_: #{
	\		  columns: ['filename'],
	\	  },
	\ },
	\ kindOptions: #{
	\ 	file: #{
	\		defaultAction: 'open',
	\	},
  \ uiParams: #{
  \   filer: #{
  \     split: 'vertical',
  \   },
  \ }}})

autocmd FileType ddu-filer call s:ddu_settings()
function! s:ddu_settings() abort
	nnoremap <buffer><silent> <CR>
		\ <Cmd>call ddu#ui#do_action('itemAction')<CR>
	nnoremap <buffer><silent> <Space>
		\ <Cmd>call ddu#ui#do_action('toggleSelectItem')<CR>
	nnoremap <buffer> o
		\ <Cmd>call ddu#ui#do_action('expandItem',
		\ #{ mode: 'toggle' })<CR>
	nnoremap <buffer><silent> q
		\ <Cmd>call ddu#ui#do_action('quit')<CR>
endfunction

nnoremap <leader>e <Cmd>call ddu#start(#{ name: 'filer' })<CR>
