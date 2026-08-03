export MISE_AUTO_ENV=true
eval "$(~/.local/bin/mise activate zsh)"
eval "$(starship init zsh)"

export EDITOR="hx"

alias cdghq='cd $(ghq root)/$(ghq list | fzf --select-1)'

dotadd() {
  local target="${1:A}"
  local config="${HOME}/.config/mise/config.toml"
  local dotfiles_dir="${config:A:h}"
  local source="home/${target#$HOME/}"

  mise -C "$dotfiles_dir" dotfiles add \
    --source "$source" \
    "$target"
}
