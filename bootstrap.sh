#!/bin/bash

# This script creates symlinks from the home directory to any desired dotfiles in ~/.dot-files

dotfiles_dir=~/.dot-files
date=`date +%Y-%m-%d--%H-%M-%S`
backupdir=~/.dot-files.bak/$date

if [ -d "$dotfiles_dir" -a ! -h "$dotfiles_dir" ]
then
	echo "Dotfiles directory exists. Skipping retrieval of this repo."
else
	git clone https://github.com/bgturner/dot-files $dotfiles_dir
fi

# Create backup of existing files.
mkdir -p $backupdir

echo "Backing up current dotfiles to: $backupdir"
mv ~/.zshrc $backupdir/
mv ~/.vimrc $backupdir/
mv ~/.tmux.conf $backupdir/
mv ~/.gitconfig $backupdir/
mv ~/.gitignore $backupdir/

echo "Creating symlinks."
ln -s $dotfiles_dir/.zshrc ~/.zshrc
ln -s $dotfiles_dir/.vimrc ~/.vimrc
ln -s $dotfiles_dir/.tmux.conf ~/.tmux.conf
ln -s $dotfiles_dir/.gitconfig ~/.gitconfig
ln -s $dotfiles_dir/.gitignore ~/.gitignore

if [ -d "$HOME/.oh-my-zsh" -a ! -h "$HOME/.oh-my-zsh" ]
then
	echo "Oh My Zsh already exists. Skipping git clone."
else
	echo "Cloning Oh My Zsh."
	git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
fi

echo "Setting Zsh as default shell."
chsh -s /bin/zsh

echo "Setting up UltiSnips snippets for Vim."
mv ~/.vim/UltiSnips $backupdir/UltiSnips-$date
ln -s $dotfiles_dir/vim/UltiSnips $HOME/.vim/UltiSnips

