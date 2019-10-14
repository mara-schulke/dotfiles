#!/bin/sh

DOTFILES=${DOTFILES:-~/dotfiles}
REPO=${REPO:-schulke-214/dotfiles}
REMOTE=${REMOTE:-git@github.com:${REPO}.git}
BRANCH=${BRANCH:-master}
USER="$(whoami)"

TITLE="
 _____     ______     ______   ______   __     __         ______     ______    
/\  __-.  /\  __ \   /\__  _\ /\  ___\ /\ \   /\ \       /\  ___\   /\  ___\   
\ \ \/\ \ \ \ \/\ \  \/_/\ \/ \ \  __\ \ \ \  \ \ \____  \ \  __\   \ \___  \  
 \ \____-  \ \_____\    \ \_\  \ \_\    \ \_\  \ \_____\  \ \_____\  \/\_____\ 
  \/____/   \/_____/     \/_/   \/_/     \/_/   \/_____/   \/_____/   \/_____/ 

"

install() {
	echo "cloning into $DOTFILES"

	git clone --depth=1 --branch "$BRANCH" "$REMOTE" "$DOTFILES" &> /dev/null || {
		echo "git clone of $REPO failed"
		exit 1
	}

	git clone https://github.com/robbyrussell/oh-my-zsh.git "$DOTFILES/zsh/oh-my-zsh" &> /dev/null || {
		echo "git clone of oh-my-zsh failed"
		exit 1
	}

	git clone https://github.com/nvm-sh/nvm.git "$DOTFILES/dependencies/nvm" &> /dev/null || {
		echo "git clone of nvm failed"
		exit 1
	}

	git clone https://github.com/lukechilds/zsh-nvm "$DOTFILES/zsh/oh-my-zsh/custom/plugins/zsh-nvm" &> /dev/null || {
		echo "git clone of zsh-nvm failed"
		exit 1
	}

	echo
}

init() {
	echo "DOTFILES=$DOTFILES" | cat - ~/.zshrc > "$DOTFILES/tmp/zshrc" && "$DOTFILES/tmp/zshrc" ~/.zshrc

	echo "source $DOTFILES/zshrc" >> ~/.zshrc
	echo "-> load $DOTFILES/zshrc in ~/.zshrc"
	# echo "-> bind aliases"
	# echo "-> setup oh-my-zsh"
	# echo "-> setup nvm"
	# echo "-> load skhd config"
}

usage() {
	echo "$TITLE"
	echo "installed $REPO for $USER :)"
}

main() {
	install
	init

	usage
}

main