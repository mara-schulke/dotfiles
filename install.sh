#!/bin/sh

DOTFILES=${DOTFILES:-~/dotfiles}
REPO=${REPO:-schulke-214/dotfiles}
REMOTE=${REMOTE:-git@github.com:${REPO}.git}
BRANCH=${BRANCH:-master}

setup_dotfiles() {
	echo "cloning dotfiles..."

	git clone --depth=1 --branch "$BRANCH" "$REMOTE" "$DOTFILES" || {
		error "git clone of the $REPO failed"
		exit 1
	}

	echo
}

main() {
	setup_dotfiles

	echo "source $DOTFILES/zshrc" >> ~/.zshrc
}

main