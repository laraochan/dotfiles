call ddu#custom#patch_local('filer', #{
	\ ui: 'filer',
  \ sources: [ #{ name: 'file', params: {} } ],
	\ sourceOptions: #{
	\ 	_: #{
  \     ignoreCase: v:true,
	\		  columns: [ 'filename' ],
  \     matchers: [ 'matcher_substring' ],
  \     converters: [ 'converter_devicon' ],
	\	  },
	\ },
	\ kindOptions: #{
	\ 	file: #{
	\		  defaultAction: 'open',
	\	  },
  \ },
  \ uiParams: #{
  \   filer: #{
  \     split: 'no',
  \   },
  \ }})

call ddu#custom#patch_local('file_search', #{
  \ ui: 'ff',
  \ sources: [
  \   #{
  \     name: 'file_external',
  \     params: #{
  \       cmd: [
  \         'rg',
  \         '--files',
  \         '--hidden',
  \         '--glob',
  \         '!.git',
  \         '--glob',
  \         '!node_modules',
  \       ],
  \     },
  \   },
  \ ],
  \ sourceOptions: #{
  \   _: #{
  \     ignoreCase: v:true,
  \     volatile: v:true,
  \     matchers: [ 'matcher_fzf' ],
  \     sorters: [ 'sorter_fzf' ],
  \     converters: [ 'converter_devicon' ],
  \   },
  \ },
  \ sourceParams: #{
  \   _: #{
  \     limit: 2000,
  \     ignoredDirectories: [ '.git', 'node_modules' ]
  \   },
  \ },
  \ filterParams: #{
  \   matcher_fzf: #{
  \     highlightMatched: 'Search',
  \   },
  \ },
  \ uiParams: #{
  \   ff: #{
  \     split: 'no',
  \   },
  \ }})
autocmd User Ddu:uiDone ++nested call ddu#ui#async_action('openFilterWindow')

call ddu#custom#patch_local('buffer', #{
  \ ui: 'ff',
  \ sources: [ #{ name: 'buffer', params: #{} } ],
  \ uiParams: #{
  \   ff: #{
  \     split: 'tab',
  \   },
  \ }})

nnoremap <leader>e <Cmd>call ddu#start(#{ name: 'filer' })<CR>
nnoremap <leader>f <Cmd>call ddu#start(#{ name: 'file_search' })<CR>
nnoremap <leader>b <Cmd>call ddu#start(#{ name: 'buffer' })<CR>

let s:mappings = #{
  \ global: [
  \   'nnoremap <buffer><silent> q <Cmd>call ddu#ui#do_action(''quit'')<CR>',
  \ ],
  \ filer: [
  \   'nnoremap <buffer><silent><expr> <CR> ddu#ui#get_item()->get("isTree", v:false) ? '
  \     .. '"<Cmd>call ddu#ui#do_action(''itemAction'', #{ name: ''narrow'' })<CR>" '
  \     .. ': "<Cmd>call ddu#ui#do_action(''itemAction'', #{ name: ''open'' })<CR>"',
  \   'nnoremap <buffer><silent> <Space> <Cmd>call ddu#ui#do_action(''toggleSelectItem'')<CR>',
  \   'nnoremap <buffer><silent> o <Cmd>call ddu#ui#do_action(''expandItem'', #{ mode: ''toggle'' })<CR>',
  \   'nnoremap <buffer><silent> .. <Cmd>call ddu#ui#do_action(''itemAction'', #{ name: ''narrow'', params: #{ path: ''..'' } })<CR>',
  \   'nnoremap <buffer><silent> c <Cmd>call ddu#ui#do_action(''itemAction'', #{ name: ''copy'' })<CR>',
  \   'nnoremap <buffer><silent> p <Cmd>call ddu#ui#do_action(''itemAction'', #{ name: ''paste'' })<CR>',
  \   'nnoremap <buffer><silent> d <Cmd>call ddu#ui#do_action(''itemAction'', #{ name: ''delete'' })<CR>',
  \   'nnoremap <buffer><silent> r <Cmd>call ddu#ui#do_action(''itemAction'', #{ name: ''rename'' })<CR>',
  \   'nnoremap <buffer><silent> m <Cmd>call ddu#ui#do_action(''itemAction'', #{ name: ''move'' })<CR>',
  \   'nnoremap <buffer><silent> n <Cmd>call ddu#ui#do_action(''itemAction'', #{ name: ''newFile'' })<CR>',
  \   'nnoremap <buffer><silent> i <Cmd>call ddu#ui#do_action(''openFilterWindow'')<CR>',
  \ ],
  \ file_search: [
  \   'nnoremap <buffer><silent> <CR> <Cmd>call ddu#ui#do_action(''itemAction'', #{ name: ''open'' })<CR>',
  \   'nnoremap <buffer><silent> i <Cmd>call ddu#ui#do_action(''openFilterWindow'')<CR>',
  \   'nnoremap <buffer><silent> <C-u> <Cmd>call ddu#ui#do_action(''previewExecute'', #{ command: ''execute "normal! \<C-y>"'' })<CR>',
  \   'nnoremap <buffer><silent> <C-d> <Cmd>call ddu#ui#do_action(''previewExecute'', #{ command: ''execute "normal! \<C-e>"'' })<CR>',
  \ ],
  \ buffer: [
  \   'nnoremap <buffer><silent> <CR> <Cmd>call ddu#ui#do_action(''itemAction'', #{ name: ''open'' })<CR>',
  \ ]}

autocmd FileType ddu* call s:apply_ddu_mappings(get(b:, 'ddu_ui_name', ''))
function! s:apply_ddu_mappings(type) abort
  for key in ['global', a:type]
    if has_key(s:mappings, key)
      for val in s:mappings[key]
        execute val
      endfor
    endif
  endfor
endfunction
