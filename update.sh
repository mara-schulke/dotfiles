#!/bin/sh

dotfiles-version() {
	cat "$DOTFILES/version"
}

dotfiles-latest-version() {
	curl -fsSL https://raw.githubusercontent.com/schulke-214/dotfiles/master/version
}

dotfiles-clear-directories() {
	rm -rf "$DOTFILES/dependencies"
	rm -rf "$DOTFILES/tmp"
}

# update functions

main() {
	echo "updating dotfiles"
	echo "$(dotfiles-version) -> $(dotfiles-next-version)"
	echo

	echo "-> clearing dependencies and temp files"
	dotfiles-clear-directories
	echo "-> downloading dotfiles $(dotfiles-latest-version)"
	git -C "$DOTFILES" pull --force
	echo "-> installing dependencies"
}

main

#### use arg to specify only deps

# dotfiles-update-deps-only() {
# 	echo "updating dotfile dependencies"
# 	echo

# 	echo "-> clearing dependencies and temp files"
# 	dotfiles-clear-directories
# 	echo "-> installing dependencies"
# }
