#!/bin/sh

DOTFILES_VERSION="$(cat '$DOTFILES/version')"

main() {
	echo "uninstalling dotfiles $DOTFILES_VERSION"

	echo "-> removing $DOTFILES"
	rm -rf "$DOTFILES"
	echo "-> removing references to dotfiles"
	sed -i -e "s/source $DOTFILES/zshrc//g" ~/.zshrc
}
main
