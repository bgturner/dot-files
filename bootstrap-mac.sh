#!/bin/bash

# Manually install certain tools
[ ! -d /opt/homebrew ] && \
    bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

[ ! -d $HOME/.oh-my-zsh ] && \
    sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

[ ! -d $HOME/.nvm ] && \
    bash -c "$(curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh)"

[ ! -f $HOME/.vim/autoload/plug.vim ] && \
    curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
	 https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim


# Configure Brew taps
brew tap homebrew/cask-versions
brew tap homebrew/cask-fonts
brew tap d12frosted/emacs-plus 

# Install various cli tools
while IFS= read -r app || [[ -n "$app" ]]; do
    brew install $app
done <<EOF
coreutils
libtool
cmake
ispell
ffmpeg
pandoc
imagemagick
grep
ripgrep
pup
librsvg
jq
fzf
asciinema
stow
direnv
pyenv
git-lfs
wakatime-cli
emacs-plus
tmux
EOF

# Install Brew Cask apps
while IFS= read -r app || [[ -n "$app" ]]; do
    brew install --cask $app
done <<EOF
alfred
around
cloudapp
syncthing
font-jetbrains-mono
font-fira-sans
font-fira-code
amethyst
1password
firefox-developer-edition
firefox brave-browser
inkscape
gimp
vlc
obs
visual-studio-code
EOF


# Link apps to play nice with Apple's default location.
[ ! -d /Applications/Emacs.app ] && \
    ln -s /opt/homebrew/opt/emacs-plus@28/Emacs.app /Applications
