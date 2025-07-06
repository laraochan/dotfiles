#!/bin/bash

# pull zsh config
rm -rf ../.zshrc
cp ~/.zshrc ../

# pull Emacs config
rm -rf ../.emacs.d
cp ~/.emacs.d/init.el ../
cp ~/.emacs.d/early-init.el ../

# pull neovim config
rm -rf ../nvim
cp -rf ~/.config/nvim ../

# pull vim config
rm -rf ../vim
cp -rf ~/.config/vim ../
