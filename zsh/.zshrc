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

export EDITOR="emacsclient -nw"

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

## Homebrew — check both prefixes so this works on Intel and Apple Silicon
if [ -d /opt/homebrew/bin ] ; then
    PATH="/opt/homebrew/bin:$PATH"
    BREW_PREFIX="/opt/homebrew"
elif [ -d /usr/local/bin ] ; then
    PATH="/usr/local/bin:$PATH"
    BREW_PREFIX="/usr/local"
fi

# Gcloud
# Add additional binary components installed via gcloud
if [ -n "$BREW_PREFIX" ]; then
    GCLOUD_PATH="$BREW_PREFIX/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/bin"
    [ -d "$GCLOUD_PATH" ] && PATH="${GCLOUD_PATH}:$PATH"
fi

# opencode (unmanaged — not a mise tool)
export PATH="/Users/ben/.opencode/bin:$PATH"

# mise — activates node/python/rust/fzf/bun/direnv/starship shims
if command -v mise >/dev/null 2>&1; then
    eval "$(mise activate zsh)"
fi

# FZF
if (( $+commands[fzf] )); then
  source <(fzf --zsh)
fi

# Direnv
if (( $+commands[direnv] )); then
    eval "$(direnv hook zsh)"
fi

# Starship prompt
if command -v starship >/dev/null 2>&1; then
    eval "$(starship init zsh)"
fi

# LifeOS launch command (added by LifeOS installer)
alias lifeos='bun /Users/ben/.claude/LIFEOS/TOOLS/lifeos.ts -s /Users/ben/.claude/LIFEOS/LIFEOS_SYSTEM_PROMPT.md'

# Source any config not tracked in git, but needed for this machine
# (e.g. CLAUDE_CODE_USE_VERTEX / CLOUD_ML_REGION on the work Mac)
if [ -f ~/.zshrc.local ]; then
    source ~/.zshrc.local
fi
