#!/bin/bash
set -euo pipefail

# Provisions the work Mac. Deliberately NOT Ansible — this machine is
# enterprise-locked-down and Ansible use there is still an open question
# (see ISA.md). Everything mise/Ansible/Stow would otherwise cover for a
# personal machine has to be installed by hand here instead.

[ ! -d /opt/homebrew ] && \
    bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# mise and stow: on a personal machine Ansible's common/linux/mac roles
# install these; there's no Ansible on the work Mac, so they're installed
# directly here instead.
cd "$(dirname "$0")"
export PATH="$HOME/.local/bin:$PATH"
[ ! -x "$HOME/.local/bin/mise" ] && curl -fsSL https://mise.run | sh

PACKAGES=(
    coreutils
    gpg
    graphviz
    grep
    ispell
    librsvg
    libtool
    neovim
    stow
)
brew install "${PACKAGES[@]}"

CASKS=(
    1password
    amethyst
    cmux
    docker
    emacs-app
    font-fira-code-nerd-font
    font-jetbrains-mono-nerd-font
    ghostty
    gcloud-cli
    syncthing
    visual-studio-code
)
brew install --cask "${CASKS[@]}"

# Stow: symlink every dotfile package into $HOME. Same step bootstrap.sh
# runs for personal machines; must land before `mise install` below, since
# the mise package is what puts config.toml at ~/.config/mise/. The work
# Mac is always Darwin, so the VS Code target is hardcoded (bootstrap.sh
# branches on uname for the Linux path).
echo "==> Symlinking dotfiles via Stow"
stow -d . -t "$HOME" zsh bash git tmux emacs vim mise ghostty starship sqlite
stow -d . -t "$HOME/Library/Application Support/Code/User" vscode

# Most CLI dev tools (rust, direnv, fzf, bun, starship, plus fd, gh, git-lfs,
# jq, pandoc, ripgrep, sqlite, tmux, kubectl, helm/helmfile, cmake,
# difftastic, imagemagick, terraform, heroku, claude-code, 1password-cli) are
# mise-managed (.config/mise/config.toml, now mise/.config/mise/config.toml
# once stowed) — not installed via brew. What's left above either needs real
# OS integration (gpg's keyring), isn't a mise concept at all (GUI casks,
# system libraries other formulae link against), or — coreutils specifically
# — mise's registry entry is uutils/coreutils (a different Rust
# reimplementation), not a substitute for GNU coreutils's gls/gcat/etc.
# kube-ps1 is dropped too: Starship's built-in kubernetes module replaces it.
# emacs-plus deliberately not pursued — plain emacs-app is what's actually
# running, has no tap-trust gate, and needs no build step. Simpler wins.

echo "==> Installing mise-managed tool versions"
mise install
