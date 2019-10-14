export GOPATH=$(go env GOPATH)
export PATH=$HOME/bin:/usr/local/bin:$PATH
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$GOPATH/bin:$PATH"

export GPG_TTY=$(tty)
export ZSH="$DOTFILES/zsh/oh-my-zsh"

# shell config
ZSH_THEME="robbyrussell"
CASE_SENSITIVE="true"
DISABLE_AUTO_TITLE="true"
DISABLE_UNTRACKED_FILES_DIRTY="true"
DISABLE_MAGIC_FUNCTIONS=true

# nvm config
NVM_DIR="$DOTFILES/dependencies/nvm"
NVM_AUTO_USE=true
NVM_LAZY_LOAD=true

plugins=(
	git
	git-flow
	jira
	npm
	nvm
	osx
)

source $ZSH/oh-my-zsh.sh
source $DOTFILES/dependencies/zsh-nvm/zsh-nvm.plugin.zsh

setopt PROMPT_SUBST
PROMPT='$(basename "$PWD")($(current_branch)) λ ' # use only current dir instead of whole path

# misc
alias static-ranger="ranger"
alias ranger="ranger --choosedir=$DOTFILES/tmp/.rangerdir; LASTDIR=`cat $DOTFILES/tmp/.rangerdir`; cd '$LASTDIR'"