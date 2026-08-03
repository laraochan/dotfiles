# larao's dotfiles

## How to setup

```sh
# Install mise
curl https://mise.run | sh
eval "$(~/.local/bin/mise activate zsh)"

# Clone this repository and open a shell in it
mise x ghq@latest -- ghq get --look -p laraochan/dotfiles

# Trust the configuration and install tools and packages
mise trust
mise install
mise bootstrap packages apply --yes

# Apply dotfiles
mise bootstrap dotfiles apply --yes

# Load the installed tools in the current shell
source ~/.zshrc
```
