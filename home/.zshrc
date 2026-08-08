export MISE_AUTO_ENV=true
eval "$(~/.local/bin/mise activate zsh)"
eval "$(starship init zsh)"

export EDITOR="hx"

alias yz="yazi"
alias lg="lazygit"
alias bp="btop"
alias hr="herdr"

cdghq() {
  local repo
  repo=$(ghq list | fzf --select-1) || return
  cd "$(ghq root)/$repo"
}

dotadd() {
  local target="${1:A}"
  local config="${HOME}/.config/mise/config.toml"
  local dotfiles_dir="${config:A:h}"
  local source="home/${target#$HOME/}"

  mise -C "$dotfiles_dir" dotfiles add \
    --source "$source" \
    "$target"
}
