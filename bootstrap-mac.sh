#!/bin/bash

#
# CLI tooling
#
echo "☻ Installing CLI tools..."
brew install coreutils
brew install cmake libtool # for compiling rando things
brew install --cask syncthing
brew install ispell
brew install ffmpeg
brew install pandoc
brew install imagemagick
brew install grep
brew install ripgrep
brew install pup
brew install librsvg
brew install jq
brew install fzf
brew install asciinema
brew install stow

#
# GUI
#
echo "☻ Installing GUI tools..."
# Fonts and themes
brew install font-fira-code

# Productivity
brew install amethyst
brew install 1password

# Browsers
brew install firefox
brew install brave-browser

# OSS graphic design
brew install inkscape
brew install gimp

# Media
brew install vlc
brew install obs

# Install Emacs
echo "☻ Installing Emacs..."
brew tap d12frosted/emacs-plus && \
    brew install emacs-plus && \
    ln -s /usr/local/opt/emacs-plus/Emacs.app /Applications/Emacs.app
