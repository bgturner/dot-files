# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh
plugins=(
    vi-mode
    wakatime
)
source $ZSH/oh-my-zsh.sh

source "$HOME/.bash_aliases"

# Appends every command to the history file once it is executed
setopt inc_append_history
# Reloads the history whenever you use it
setopt share_history

# User configuration
export fpath=(~/.zsh/completionA $fpath)
autoload -Uz compinit && compinit

# To see available colors, run this command: spectrum_ls
PROMPT='%(?.%F{green}★.%F{red}✗ %?)%f $(git_prompt_info) %B%F{240}%2~%f%b %# '

# Settings for git theming of the prompt
ZSH_THEME_GIT_PROMPT_PREFIX="%F{141}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%f"
ZSH_THEME_GIT_PROMPT_CLEAN=""
ZSH_THEME_GIT_PROMPT_DIRTY="%F{009}*%f"

# LS colors, made with http://geoff.greer.fm/lscolors/
export LSCOLORS="Gxfxcxdxbxegedabagacad"
export LS_COLORS='no=00:fi=00:di=01;34:ln=00;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=41;33;01:ex=00;32:*.cmd=00;32:*.exe=01;32:*.com=01;32:*.bat=01;32:*.btm=01;32:*.dll=01;32:*.tar=00;31:*.tbz=00;31:*.tgz=00;31:*.rpm=00;31:*.deb=00;31:*.arj=00;31:*.taz=00;31:*.lzh=00;31:*.lzma=00;31:*.zip=00;31:*.zoo=00;31:*.z=00;31:*.Z=00;31:*.gz=00;31:*.bz2=00;31:*.tb2=00;31:*.tz2=00;31:*.tbz2=00;31:*.avi=01;35:*.bmp=01;35:*.fli=01;35:*.gif=01;35:*.jpg=01;35:*.jpeg=01;35:*.mng=01;35:*.mov=01;35:*.mpg=01;35:*.pcx=01;35:*.pbm=01;35:*.pgm=01;35:*.png=01;35:*.ppm=01;35:*.tga=01;35:*.tif=01;35:*.xbm=01;35:*.xpm=01;35:*.dl=01;35:*.gl=01;35:*.wmv=01;35:*.aiff=00;32:*.au=00;32:*.mid=00;32:*.mp3=00;32:*.ogg=00;32:*.voc=00;32:*.wav=00;32:*.patch=00;34:*.o=00;32:*.so=01;35:*.ko=01;31:*.la=00;33'
export CLICOLOR=1

# Preferred editor for local and remote sessions
export EDITOR='vim'

# Enable vi-style keybindings
bindkey -v

# allow v to edit the command line (standard behaviour)
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey -M vicmd 'v' edit-command-line


# Add Global Composer to path
[ -d "$HOME/.config/composer/vendor/bin" ] && export PATH="$HOME/.config/composer/vendor/bin:${PATH}"


# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

# Configure python dev environment
if [ -d "$HOME/.pyenv" ]; then
    export PYENV_ROOT="$HOME/.pyenv"
    export PATH="$PYENV_ROOT/bin:$PATH"
fi
if command -v pyenv 1>/dev/null 2>&1; then
    eval "$(pyenv init --path)"
fi

# Configure node dev environment
if [ -d "$HOME/.nvm" ]; then
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
fi

# Use FZF if installed
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

eval "$(direnv hook zsh)"

export NVS_HOME="$HOME/.nvs"
[ -s "$NVS_HOME/nvs.sh" ] && . "$NVS_HOME/nvs.sh"
