dotfiles-version() {
	cat "$DOTFILES/version"
}

main() {
	echo "uninstalling dotfiles $(dotfiles-version)"

	echo "-> removing $DOTFILES"
	rm -rf "$DOTFILES"
	echo "-> removing references to dotfiles"
	sed -i -e "s/source $DOTFILES/zshrc//g" ~/.zshrc
}
main
