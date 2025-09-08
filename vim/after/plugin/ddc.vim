call ddc#custom#patch_global('ui', 'native')
call ddc#custom#patch_global('sources', ['lsp', 'around'])
call ddc#custom#patch_global('sourceOptions', #{
  \ _: #{
  \   matchers: ['matcher_head'],
  \   sorters: ['sorter_rank'],
  \ },
  \ around: #{
  \   mark: '[AROUND]'
  \ },
  \ lsp: #{
  \   mark: '[LSP]',
  \   isVolatile: v:true,
  \   forceCompletionPattern: '\.\w*|:\w*|->\w*',
  \ }})
call ddc#custom#patch_global('sourceParams', #{
  \ lsp: #{
  \   enableResolveItem: v:true,
  \   enableAdditionalTextEdit: v:true,
  \   lspEngine: 'vim-lsp'
  \ }})
call ddc#custom#patch_global('autoCompleteDelay', 100)
call ddc#enable()
call popup_preview#enable()
let g:signature_help_config = #{
      \ contentsStyle: "full",
      \ viewStyle: "floating"
      \ }
let g:lsp_signature_help_enabled = 0
call signature_help#enable()

