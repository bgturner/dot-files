source "$HOME/.bash_aliases"

# Appends every command to the history file once it is executed
setopt inc_append_history

# except for commands that start with a space
setopt HIST_IGNORE_SPACE

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

## Prompt
# 1. Load the module
autoload -Uz vcs_info
setopt PROMPT_SUBST

# 2. Configure vcs_info to check for changes
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git:*' check-for-changes true

# 3. Define the purple format and the red asterisk for unstaged changes
# %b = branch name, %u = unstaged (dirty) string
zstyle ':vcs_info:git:*' formats ' %F{magenta}%b%u%F{magenta}%f'
zstyle ':vcs_info:git:*' unstagedstr '%F{red}*%f'

# 4. Run vcs_info before every prompt
precmd() { vcs_info }

# Include kube_ps1 if present
# See: https://github.com/jonmosco/kube-ps1
KUBE_PS1_PATH="/opt/homebrew/opt/kube-ps1/share/kube-ps1.sh"
if [ -f "$KUBE_PS1_PATH" ]; then
    source "$KUBE_PS1_PATH"
    # Create a helper function that only runs if kube_ps1 was successfully sourced
    _render_kube_ps1() { kube_ps1 }
else
    # Empty function so the prompt doesn't break if kube_ps1 is missing
    _render_kube_ps1() { : }
fi

# 5. Your existing status logic + the new vcs_info variable
STATUS_INDICATOR='%(?.%F{green}◉%f.%F{red}! %?%f)'
PROMPT='${STATUS_INDICATOR} %~${vcs_info_msg_0_} $(_render_kube_ps1)%# '

## Tools
if [ -d /opt/homebrew/bin ] ; then
    PATH="/opt/homebrew/bin:$PATH"
fi

# Gcloud
# Add additional binary components installed via gcloud
GCLOUD_PATH='/opt/homebrew/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/bin/'
if [ -d "$GCLOUD_PATH" ]; then
    export PATH="${GCLOUD_PATH}:$PATH"
fi

if (( $+commands[fnm] )); then
  source <(fnm env --use-on-cd --shell zsh)
fi

# Source any config not tracked in git, but needed for this machine
if [ -f ~/.zshrc.local ]; then
    source ~/.zshrc.local
fi
