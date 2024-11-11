#!/bin/bash

# Manually install certain tools
[ ! -d /opt/homebrew ] && \
    bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

[ ! -d $HOME/.oh-my-zsh ] && \
    sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

if ! command -v fnm &> /dev/null
then
    curl -fsSL https://fnm.vercel.app/install | bash
fi

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
asciinema
cmake
coreutils
direnv
difftastic
emacs-plus
fd
ffmpeg
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
pup
pyenv
ripgrep
shellcheck
sqlite
stow
tmux
wakatime-cli
EOF

# Install Brew Cask apps
while IFS= read -r app || [[ -n "$app" ]]; do
    brew install --cask $app
done <<EOF
alfred
cloudapp
docker
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
parallels
visual-studio-code
EOF


# Link apps to play nice with Apple's default location.
[ ! -d /Applications/Emacs.app ] && \
    ln -s /opt/homebrew/opt/emacs-plus@28/Emacs.app /Applications
