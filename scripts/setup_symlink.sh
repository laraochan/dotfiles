#!/bin/bash
set -eu

echo "Start: setup_symlink.sh"

DOTFILES_DIR="$(cd "$(dirname "$0")" && pwd)"
CONFIG_DIR="$HOME/.config"
case "$(uname -s)" in
  "Darwin")
    echo "macOS detected"
    OS_TYPE="mac"
    ;;
  "Linux")
    echo "Linux detected"
    OS_TYPE="linux"
    ;;
  *)
    echo "Unsupported OS: $(uname -s)"
esac

COMMON_DIRS=(
  vim
  nvim
  emacs
  zellij
  alacritty
)

MAC_DIRS=(
  aerospace
  yabai
  skhd
)

LINUX_DIRS=(
  sway
  waybar
  mako
)

if [[ -e "$HOME/.zshrc" || -L "$HOME/.zshrc" ]]; then
  read -n 1 -p "$HOME/.zshrc already exists. Delete it and create a link? [y/N] " answer
  echo
  if [[ "$answer" =~ ^[yY]$ ]]; then
    rm "$HOME/.zshrc"
    case "$OS_TYPE" in
      "mac")
        echo "$DOTFILES_DIR/zsh/mac → $HOME/.zshrc"
        ln -s "$DOTFILES_DIR/zsh/mac" "$HOME/.zshrc"
        ;;
      "linux")
        echo "$DOTFILES_DIR/zsh/linux → $HOME/.zshrc"
        ln -s "$DOTFILES_DIR/zsh/linux" "$HOME/.zshrc"
        ;;
    esac
  else
    echo "Skipped: $CONFIG_DIR/zsh"
  fi
else
  case "$OS_TYPE" in
    "mac")
      echo "$DOTFILES_DIR/zsh/mac → $HOME/.zshrc"
      ln -s "$DOTFILES_DIR/zsh/mac" "$HOME/.zshrc"
      ;;
    "linux")
      echo "$DOTFILES_DIR/zsh/linux → $HOME/.zshrc"
      ln -s "$DOTFILES_DIR/zsh/linux" "$HOME/.zshrc"
      ;;
  esac
fi

DIRS=("${COMMON_DIRS[@]}")
if [[ "$OS_TYPE" = "mac" ]]; then
  DIRS+=("${MAC_DIRS[@]}")
elif [[ "$OS_TYPE" = "linux" ]]; then
  DIRS+=("${LINUX_DIRS[@]}")
fi

for dir in "${DIRS[@]}"; do
  src="$DOTFILES_DIR/$dir"
  dest="$CONFIG_DIR/$dir"

  if [[ -e "$dest" || -L "$dest" ]]; then
    read -n 1 -p "$dest already exists. Delete it and create a link? [y/N] " answer
    echo
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

echo "End: setup_symlink.sh"
