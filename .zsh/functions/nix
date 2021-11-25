nix-shell-active() {
	if [[ ${#shellHook} -gt 0 || -n $IN_NIX_SHELL ]]; then
		return 0;
	else
		return 1;
	fi
}
