#!/bin/bash

dot_file_folder="$(pwd)"

function goto_config_dir {
  mac_vscode_config="${HOME}/Library/Application Support/Code/User" 
  [ -d  "$mac_vscode_config" ] && cd "$mac_vscode_config" && echo "Changed to ${mac_vscode_config}"
  linux_vscode_config="${HOME}/.config/Code/User"
  [ -d  "$linux_vscode_config" ] && cd "$linux_vscode_config" && echo "Changed to ${linux_vscode_config}"
}

function backup_configs {
  echo "Backing up existing configs"
  date_slug="$(date "+%Y%M%d%H%m%S")"
    cp settings{,.${date_slug}}.json
    cp keybindings{,.${date_slug}}.json
}

function link_dotfile_configs {
  echo "Linking config from dotfiles"
  rm settings.json keybindings.json && \
    ln -s "${dot_file_folder}/settings.json" ./ && \
    ln -s "${dot_file_folder}/keybindings.json" ./
}

goto_config_dir && backup_configs && link_dotfile_configs
