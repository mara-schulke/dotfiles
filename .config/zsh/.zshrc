export PATH="/usr/local/bin:$PATH"
export PATH="/opt/local/bin:$PATH"
export PATH="/opt/local/sbin:$PATH"
export PATH="/opt/homebrew/bin:$PATH"
export PATH="$HOME/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.config/zsh/utils:$PATH"

export GPG_TTY=$(tty)
export ZSH="$HOME/.config/zsh/oh-my-zsh"
export ZSH_DISABLE_COMPFIX=true

# load plugin configuration
source $HOME/.config/zsh/plugins

# load dependencies
source $HOME/.config/zsh/oh-my-zsh/oh-my-zsh.sh

# load functions
source $HOME/.config/zsh/functions/github
source $HOME/.config/zsh/functions/java
source $HOME/.config/zsh/functions/nix
source $HOME/.config/zsh/functions/net
source $HOME/.config/zsh/functions/work

# load configuration
source $HOME/.config/zsh/prompt
source $HOME/.config/zsh/aliases
source $HOME/.config/zsh/environment

# go into the current project directory if given
work restore
