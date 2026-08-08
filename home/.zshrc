export MISE_AUTO_ENV=true
eval "$(~/.local/bin/mise activate zsh)"
eval "$(starship init zsh)"

export EDITOR="hx"

HISTFILE="$HOME/.zsh_history"
HISTSIZE=10000
SAVEHIST=10000

setopt SHARE_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_SAVE_NO_DUPS
setopt HIST_REDUCE_BLANKS
setopt HIST_IGNORE_SPACE
setopt AUTO_CD
setopt AUTO_PUSHD
setopt PUSHD_IGNORE_DUPS
setopt INTERACTIVE_COMMENTS
setopt NO_BEEP

autoload -Uz compinit
compinit
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

bindkey -e
source <(fzf --zsh)
eval "$(zoxide init zsh)"

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
