#!/bin/bash

# This script creates symlinks from the home directory to the various dotfiles in ~/.dot-files

dotfiles_dir=~/.dot-files
date=`date +%Y-%m-%d--%H-%M-%S`
backupdir=~/.dot-files.bak/$date

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

