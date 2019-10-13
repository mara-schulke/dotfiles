#!/bin/sh

export GOPATH=$(go env GOPATH)
export PATH=$HOME/bin:/usr/local/bin:$PATH
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$GOPATH/bin:$PATH"

export GPG_TTY=$(tty)
export ZSH="$PWD/zsh/oh-my-zsh"

ZSH_THEME="robbyrussell"

CASE_SENSITIVE="true"
DISABLE_AUTO_TITLE="true"
DISABLE_UNTRACKED_FILES_DIRTY="true"
DISABLE_MAGIC_FUNCTIONS=true

plugins=(
  git
  git-flow
  jira
  npm
  osx
)

source $ZSH/oh-my-zsh.sh
source ./scripts/nvm.sh

setopt PROMPT_SUBST
PROMPT='%~/ {$(current_branch)} :: λ'