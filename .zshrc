export GOPATH=$(go env GOPATH)
export PATH=$HOME/bin:/usr/local/bin:$PATH
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$GOPATH/bin:$PATH"

export GPG_TTY=$(tty)
export ZSH="$HOME/.oh-my-zsh"

if systemctl -q is-active graphical.target && [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
	exec startx
fi

# load plugin configuration
source $HOME/.zsh/plugins

# load dependencies
source $HOME/.oh-my-zsh/oh-my-zsh.sh
source $HOME/.zsh-nvm/zsh-nvm.plugin.zsh

# load configuration
source $HOME/.zsh/prompt
source $HOME/.zsh/aliases

clear
