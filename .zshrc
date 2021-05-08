export GOPATH=$(go env GOPATH)
export PATH="$HOME/bin:$PATH"
export PATH="/usr/local/bin:$PATH"
export PATH="/usr/local/sbin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="/usr/local/opt/python/libexec/bin:$PATH"
export PATH="$GOPATH/bin:$PATH"
export PATH="$PATH:/usr/local/mysql/bin"

export GPG_TTY=$(tty)
export ZSH="$HOME/.oh-my-zsh"
export ZSH_DISABLE_COMPFIX=true

# load plugin configuration
source $HOME/.zsh/plugins

# load functions
source $HOME/.zsh/functions/github
source $HOME/.zsh/functions/java
source $HOME/.zsh/functions/nix
source $HOME/.zsh/functions/work

# load dependencies
source $HOME/.oh-my-zsh/oh-my-zsh.sh

# load configuration
source $HOME/.zsh/prompt
source $HOME/.zsh/aliases

source $HOME/.zsh/cfg/environment
source $HOME/.zsh/cfg/postgres

# go into the current project directory if given
work restore

if systemctl -q is-active graphical.target && [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
	exec startx
fi
