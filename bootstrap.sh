#!/bin/bash
set -euo pipefail

# Orchestrates provisioning a personal machine: Ansible (OS packages,
# system setup) -> Stow (dotfile symlinks) -> mise (tool versions).
#
# Install logic doesn't live here — see provision/personal/ for what
# Ansible installs, and .config/mise/config.toml (now mise/.config/mise/
# once stowed) for tool versions. This script only sequences them.
#
# Not for the work Mac — see bootstrap-mac.sh.

cd "$(dirname "$0")"
export PATH="$HOME/.local/bin:$PATH"

echo "==> Provisioning via Ansible (provision/personal)"
(
  cd provision/personal
  ansible-galaxy collection install -r requirements.yml
  ansible-playbook -i inventory playbook.yml --limit "$(hostname -s)" --ask-become-pass
)

echo "==> Symlinking dotfiles via Stow"
stow -d . -t "$HOME" zsh bash git tmux emacs vim mise ghostty starship sqlite

case "$(uname)" in
  Darwin)
    stow -d . -t "$HOME/Library/Application Support/Code/User" vscode
    ;;
  Linux)
    stow -d . -t "$HOME/.config/Code/User" vscode
    ;;
esac

echo "==> Installing mise-managed tool versions"
mise install
