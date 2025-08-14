#!/bin/bash
set -eu

DOTFILES_DIR="$(cd "$(dirname "$0")" && pwd)"
CONFIG_DIR="$HOME/.config"

if [[ -e "$HOME/.zshrc" || -L "$HOME/.zshrc" ]]; then
  read -p "$HOME/.zshrc already exists. Delete it and create a link? [y/N] " answer
  if [[ "$answer" =~ ^[yY]$ ]]; then
    rm "$HOME/.zshrc"
    ln -s "$DOTFILES_DIR/.zshrc" "$HOME/.zshrc"
  else
    echo "Skipped: $HOME/.zshrc"
  fi
fi

DIRS=(
  nvim
  zellij
  alacritty
  aerospace
)

for dir in "${DIRS[@]}"; do
  src="$DOTFILES_DIR/$dir"
  dest="$CONFIG_DIR/$dir"

  if [[ -e "$dest" || -L "$dest" ]]; then
    read -p "$dest already exists. Delete it and create a link? [y/N] " answer
    if [[ "$answer" =~ ^[yY]$ ]]; then
      rm -rf "$dest"
    else
      echo "Skipped: $dest"
      continue
    fi
  fi
  
  echo "$dest → $src"
  ln -s "$src" "$dest"
done

echo "Done!"
