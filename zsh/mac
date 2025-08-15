eval "$(starship init zsh)"

eval "$(/opt/homebrew/bin/brew shellenv)"

eval "$(nodenv init -)"

export PATH="${AQUA_ROOT_DIR:-${XDG_DATA_HOME:-$HOME/.local/share}/aquaproj-aqua}/bin:$PATH"

export GOPATH=$HOME/go
export GOBIN=$GOPATH/bin
export PATH=$PATH:$GOBIN

alias gd='cd "$(ghq list --full-path | peco)"'
alias sd='ssh "$(grep -w Host ~/.ssh/config | grep -v "\*" | awk "{print \$2}" | peco)"'

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
