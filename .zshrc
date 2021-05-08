export PATH="$HOME/bin:$PATH"
export PATH="$HOME/.zsh/utils:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"

export GPG_TTY=$(tty)
export ZSH="$HOME/.oh-my-zsh"
export ZSH_DISABLE_COMPFIX=true

# load plugin configuration
source $HOME/.zsh/plugins

# load dependencies
source $HOME/.oh-my-zsh/oh-my-zsh.sh

# load functions
source $HOME/.zsh/functions/github
source $HOME/.zsh/functions/java
source $HOME/.zsh/functions/nix
source $HOME/.zsh/functions/work

# load configuration
source $HOME/.zsh/prompt
source $HOME/.zsh/aliases
source $HOME/.zsh/environment

# go into the current project directory if given
work restore

# if systemctl -q is-active graphical.target && [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
# 	exec startx
# fi
