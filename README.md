# larao's dotfiles

## How to setup

```sh
# Install mise
curl https://mise.run | sh
eval "$(~/.local/bin/mise activate zsh)"

# Enable platform-specific config such as mise.macos.toml / mise.linux.toml
export MISE_AUTO_ENV=true

# Clone this repository and open a shell in it
mise x ghq@latest -- ghq get --look -p laraochan/dotfiles

# Trust the configuration and install tools and packages
mise trust
mise install
mise bootstrap packages apply --yes

# Apply dotfiles
mise bootstrap dotfiles apply --yes

# Load the installed shell configuration
source ~/.zshrc
```
