export GOPATH=$(go env GOPATH)

export PATH="$HOME/bin:/usr/local/bin:$PATH"
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
source $HOME/.zsh/functions

# load dependencies
source $HOME/.oh-my-zsh/oh-my-zsh.sh

# load configuration
source $HOME/.zsh/prompt
source $HOME/.zsh/aliases

source $HOME/.zsh/cfg/environment
source $HOME/.zsh/cfg/postgres


# if [ OS = "linux" ] || [ OS = "linux2" ]; then
#	if systemctl -q is-active graphical.target && [ ! $DISPLAY && $XDG_VTNR -eq 1 ]; then
#		exec startx
#	fi
# fi

if which pyenv > /dev/null; then
	eval "$(pyenv init -)"
	pyenv virtualenvwrapper
fi
