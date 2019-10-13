#!/bin/sh

DOTFILES=${DOTFILES:-~/dotfiles}
REPO=${REPO:-schulke-214/dotfiles}
REMOTE=${REMOTE:-git@github.com:${REPO}.git}
BRANCH=${BRANCH:-master}

setup_dotfiles() {
	echo "cloning dotfiles..."

	git clone --depth=1 --branch "$BRANCH" "$REMOTE" "$DOTFILES" &>/dev/null || {
		error "git clone of the $REPO failed"
		exit 1
	}

	git clone https://github.com/nvm-sh/nvm.git "$DOTFILES/dependencies/nvm" &>/dev/null || {
		error "git clone of nvm failed"
		exit 1
	}

	git clone https://github.com/lukechilds/zsh-nvm "$DOTFILES/zsh/oh-my-zsh/custom/plugins/zsh-nvm" &>/dev/null || {
		error "git clone of zsh-nvm failed"
		exit 1
	}

	echo
}

main() {
	setup_dotfiles

	echo "source $DOTFILES/zshrc" >> ~/.zshrc
}

main