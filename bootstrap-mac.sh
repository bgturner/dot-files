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
[ ! -x "$HOME/.local/bin/mise" ] && curl -fsSL https://mise.run | sh

# Configure Brew taps
brew tap hashicorp/tap
brew tap heroku/brew

PACKAGES=(
    cmake
    coreutils
    difftastic
    ffmpeg
    fd
    gh
    git-lfs
    gpg
    graphviz
    grep
    helm
    helmfile
    heroku
    imagemagick
    ispell
    jq
    kubectl
    librsvg
    libtool
    neovim
    pandoc
    ripgrep
    sqlite
    stow
    hashicorp/tap/terraform
    tmux
)
brew install "${PACKAGES[@]}"

CASKS=(
    1password
    1password-cli
    amethyst
    claude-code
    docker
    font-fira-code-nerd-font
    font-jetbrains-mono-nerd-font
    ghostty
    gcloud-cli
    syncthing
    visual-studio-code
)
brew install --cask "${CASKS[@]}"

# Rust, direnv, fzf, bun, and Starship are mise-managed (.config/mise/config.toml,
# now mise/.config/mise/config.toml once stowed) — not installed via brew.
# kube-ps1 is dropped too: Starship's built-in kubernetes module replaces it.
# emacs-plus-app is NOT installed here — that cask name no longer resolves
# under d12frosted/emacs-plus (now versioned source-build formulae behind a
# brew-trust gate). See ISA.md Decisions for what's needed to re-add it.
