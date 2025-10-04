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

autocmd FileType ddu-filer call s:ddu_filer_setting()
function! s:ddu_filer_setting() abort
  nnoremap <buffer><silent><expr> <CR>
    \ ddu#ui#get_item()->get('isTree', v:false) ?
    \ "<Cmd>call ddu#ui#do_action('itemAction', #{ name: 'narrow' })<CR>" :
    \ "<Cmd>call ddu#ui#do_action('itemAction', #{ name: 'open' })<CR>"
	nnoremap <buffer><silent> <Space>
		\ <Cmd>call ddu#ui#do_action('toggleSelectItem')<CR>
	nnoremap <buffer> o
		\ <Cmd>call ddu#ui#do_action('expandItem',
		\ #{ mode: 'toggle' })<CR>
	nnoremap <buffer><silent> q
		\ <Cmd>call ddu#ui#do_action('quit')<CR>
  nnoremap <buffer><silent> ..
    \ <Cmd>call ddu#ui#do_action('itemAction', #{ name: 'narrow', params: #{ path: '..' } })<CR>
  nnoremap <buffer><silent> c
    \ <Cmd>call ddu#ui#do_action('itemAction', #{ name: 'copy' })<CR>
  nnoremap <buffer><silent> p
    \ <Cmd>call ddu#ui#do_action('itemAction', #{ name: 'paste' })<CR>
  nnoremap <buffer><silent> d
    \ <Cmd>call ddu#ui#do_action('itemAction', #{ name: 'delete' })<CR>
  nnoremap <buffer><silent> r
    \ <Cmd>call ddu#ui#do_action('itemAction', #{ name: 'rename' })<CR>
  nnoremap <buffer><silent> m
    \ <Cmd>call ddu#ui#do_action('itemAction', #{ name: 'move' })<CR>
  nnoremap <buffer><silent> n
    \ <Cmd>call ddu#ui#do_action('itemAction', #{ name: 'newFile' })<CR>
endfunction

nnoremap <leader>e <Cmd>call ddu#start(#{ name: 'filer' })<CR>

