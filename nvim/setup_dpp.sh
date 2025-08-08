#!/bin/bash

BASE_DIR="$HOME/.cache/nvim/dpp/repos/github.com"

mkdir -p "$BASE_DIR"

REPOS=(
    "vim-denops/denops.vim"
    "vim-denops/denops-helloworld.vim"
    "Shougo/dpp.vim"
    "Shougo/dpp-ext-installer"
    "Shougo/dpp-ext-local"
    "Shougo/dpp-ext-toml"
    "Shougo/dpp-ext-lazy"
    "Shougo/dpp-protocol-git"
)

for repo in "${REPOS[@]}"; do
    dest="$BASE_DIR/$repo"
    if [ -d "$dest" ]; then
        echo "Already exists: $dest"
    else
        echo "Cloning $repo..."
        mkdir -p "$(dirname "$dest")"
        git clone "https://github.com/$repo.git" "$dest"
    fi
done

echo "Done!"

