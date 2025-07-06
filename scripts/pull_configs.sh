#!/bin/bash

# pull zsh config
cp -f ~/.zshrc ../

# pull Emacs config
rm -rf ../.emacs.d
cp ~/.emacs.d/init.el ../
cp ~/.emacs.d/early-init.el ../

# pull neovim config
cp -rf ~/.config/nvim ../

# pull vim config
cp -rf ~/.config/vim ../
