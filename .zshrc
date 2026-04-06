source "$HOME/.bash_aliases"

# Appends every command to the history file once it is executed
setopt inc_append_history

# Reloads the history whenever you use it
setopt share_history

# User configuration
export fpath=(~/.zsh/completionA $fpath)
autoload -Uz compinit && compinit

export CLICOLOR=1

# Enable vi-style keybindings in regular shell
bindkey -v

# allow v to edit the command line (standard behaviour)
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey -M vicmd 'v' edit-command-line

## Add common bin locations to PATH
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

# FZF https://github.com/junegunn/fzf
if (( $+commands[fzf] )); then
  source <(fzf --zsh)
fi

# Direnv
if (( $+commands[direnv] )); then
    eval "$(direnv hook zsh)"
fi
