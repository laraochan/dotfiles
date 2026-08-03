# larao's dotfiles

Personal dotfiles managed with [chezmoi](https://www.chezmoi.io/) and [mise](https://mise.jdx.dev/).

## How to setup

```sh
# Install mise
curl https://mise.run | sh

# Apply dotfiles
~/.local/bin/mise x chezmoi@latest -- \
  chezmoi --source "$(ghq root)/github.com/larao/dotfiles" init --apply
source ~/.zshrc

# Install tools and packages
mise install
mise bootstrap packages apply
```
