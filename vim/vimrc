set regexpengine=2
set shiftround
set noswapfile
set nocompatible
set smartindent

augroup FileConfig
autocmd!
autocmd FileType go setlocal tabstop=4 shiftwidth=4 softtabstop=4
autocmd FileType typescript,typescriptreact setlocal tabstop=2 shiftwidth=2 softtabstop=2 expandtab
augroup END

syntax on

const s:denops_src = '~/.cache/dpp/repos/github.com/vim-denops/denops.vim'
const s:denops_hello = '~/.cache/dpp/repos/github.com/vim-denops/denops-helloworld.vim'

const s:dpp_src = '~/.cache/dpp/repos/github.com/Shougo/dpp.vim'
const s:dpp_installer = '~/.cache/dpp/repos/github.com/Shougo/dpp-ext-installer'
const s:dpp_git = '~/.cache/dpp/repos/github.com/Shougo/dpp-protocol-git'

const s:dpp_base = '~/.cache/dpp/'
const s:dpp_config = '~/.config/vim/dpp.ts'

execute 'set runtimepath+=' .. s:denops_src
execute 'set runtimepath+=' .. s:denops_hello
execute 'set runtimepath+=' .. s:dpp_src
execute 'set runtimepath+=' .. s:dpp_installer
execute 'set runtimepath+=' .. s:dpp_git

if s:dpp_base->dpp#min#load_state()
        autocmd User DenopsReady
        \ : echohl WarningMsg
        \ | echomsg 'dpp load_state() is failed'
        \ | echohl NONE
        \ | call dpp#make_state(s:dpp_base, s:dpp_config)
endif

autocmd User Dpp:makeStatePost
        \ : echohl WarningMsg
        \ | echomsg 'dpp make_state() is done'
        \ | echohl NONE
