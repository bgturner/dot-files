#!/bin/bash

# Manually install certain tools
[ ! -d /opt/homebrew ] && \
    bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

[ ! -f $HOME/.vim/autoload/plug.vim ] && \
    curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
	 https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# Configure Brew taps
brew tap d12frosted/emacs-plus 
brew tap heroku/brew
brew tap oven-sh/bun

PACKAGES=(
    1password-cli
    bun
    cmake
    coreutils
    difftastic
    direnv
    fd
    ffmpeg
    fnm
    fzf
    gh
    git-lfs
    graphviz
    grep
    heroku
    imagemagick
    ispell
    jq
    librsvg
    libtool
    neovim
    pandoc
    ripgrep
    sqlite
    tmux
)
brew install "${PACKAGES[@]}"

CASKS=(
    1password
    amethyst
    docker
    emacs-plus-app
    font-fira-code-nerd-font
    font-jetbrains-mono-nerd-font
    ghostty
    google-cloud-sdk
    syncthing
    visual-studio-code
)
brew install --cask "${CASKS[@]}"
