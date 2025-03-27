#!/bin/bash

# This script creates symlinks from the home directory to the various dotfiles in ~/.dot-files


heading () {
	echo ""
	echo "*******************************************************************************"
	echo "$1"
	echo "*******************************************************************************"
}

heading "Backing up and linking dot files..."

dotfiles_dir=~/.dot-files
date=`date +%Y-%m-%d--%H-%M-%S`
backupdir=~/${dotfiles_dir}.bak/$date

function backup_file {
  srcFile="$HOME/${1}"
  mv "${srcFile}" "${backupdir}"
}

function link_file {
  file="${1}"
  ln -s "$(pwd)/${file}" "${HOME}/${file}"
}

while IFS= read -r slug || [[ -n "$slug" ]]; do
  if [[ ! -L "${HOME}/${slug}" ]]; then
    backup_file $slug
    link_file $slug
  else
    echo "Skipping: ${slug}"
  fi
done <<EOF
.bash_aliases
.bash_prompt
.bashrc
.emacs.d
.gitconfig
.inputrc
.profile
.sqliterc
.tmux.conf
.vim
.zshrc
EOF


if [ -d "$HOME/.oh-my-zsh" -a ! -h "$HOME/.oh-my-zsh" ]
then
	echo "Skipping: Oh My Zsh already exists."
else
	echo "Cloning Oh My Zsh."
	git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
fi

if [ -d "$HOME/.fzf" -a ! -h "$HOME/.fzf" ]
then
	echo "Skipping: FZF already installed."
else
	echo "Cloning FZF"
	git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf && ~/.fzf/install
fi

if [ -d "$HOME/.cargo" ]; then
	echo "Skipping: Rust already installed."
else
	echo "Installing Rust Cargo"
  curl https://sh.rustup.rs -sSf | sh
fi

if [ -f "$HOME/.vim/autoload/plug.vim" -a ! -h "$HOME/.vim/autoload/plug.vim" ]
then
	echo "Skipping: Vim Plug already installed."
else
  echo "Cloning Vim Plug"
	curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
		https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi

## Ensure apps

heading "Installing Nerd Fonts"

## Ensure Fonts
install_nerd_font () {
	nerd_font_name="$1"
	nerd_font_version="v3.1.1"

	# TODO: guard for already existing font
	echo "Installing ${nerd_font_name}"

	(
		mkdir -p ~/.fonts
		cd ~/.fonts

		curl -fLo "${nerd_font_name}.zip" \
			"https://github.com/ryanoasis/nerd-fonts/releases/download/${nerd_font_version}/${nerd_font_name}.zip" \
			&& unzip -d "${nerd_font_name}" "${nerd_font_name}.zip" \
			&& mv "${nerd_font_name}/"*.ttf ./ \
			&& rm "${nerd_font_name}.zip"
	)
}

install_nerd_font "JetBrainsMono"
install_nerd_font "SourceCodePro" # wtf, SauceCodePro?

fc-cache -f -v

heading "Ensure required apps are installed"

sudo apt install git git-lfs direnv wget tmux jq pandoc build-essential

if [ -d "$HOME/.krew" ]; then
	echo "Skipping: Krew already installed."
else
    echo "Installing Krew"
    (
        set -x; cd "$(mktemp -d)" &&
        OS="$(uname | tr '[:upper:]' '[:lower:]')" &&
        ARCH="$(uname -m | sed -e 's/x86_64/amd64/' -e 's/\(arm\)\(64\)\?.*/\1\2/' -e 's/aarch64$/arm64/')" &&
        KREW="krew-${OS}_${ARCH}" &&
        curl -fsSLO "https://github.com/kubernetes-sigs/krew/releases/latest/download/${KREW}.tar.gz" &&
        tar zxvf "${KREW}.tar.gz" &&
        ./"${KREW}" install krew

        kubectl krew install ctx
        kubectl krew install ns
    )
fi
