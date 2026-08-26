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
export PATH="$HOME/.local/bin:$PATH"
[ ! -x "$HOME/.local/bin/mise" ] && curl -fsSL https://mise.run | sh

PACKAGES=(
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
    docker
    font-fira-code-nerd-font
    font-jetbrains-mono-nerd-font
    ghostty
    gcloud-cli
    syncthing
    visual-studio-code
)
brew install --cask "${CASKS[@]}"

# Most CLI dev tools (rust, direnv, fzf, bun, starship, plus fd, gh, git-lfs,
# jq, pandoc, ripgrep, sqlite, tmux, kubectl, helm/helmfile, cmake, coreutils,
# difftastic, imagemagick, terraform, heroku, claude-code, 1password-cli) are
# mise-managed (.config/mise/config.toml, now mise/.config/mise/config.toml
# once stowed) — not installed via brew. What's left above either needs real
# OS integration (gpg's keyring) or isn't a mise concept at all (GUI casks,
# system libraries other formulae link against).
# kube-ps1 is dropped too: Starship's built-in kubernetes module replaces it.
# emacs-plus-app is NOT installed here — that cask name no longer resolves
# under d12frosted/emacs-plus (now versioned source-build formulae behind a
# brew-trust gate). See ISA.md Decisions for what's needed to re-add it.

echo "==> Installing mise-managed tool versions"
mise install
