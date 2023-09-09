work_help() { command cat << EOF
work 2.0.0
Mara Schulke <mara@schulke.xyz>
A helper to manage the root dir a new shell is spawned in

USAGE:
    work <SUBCOMMAND> <PROJECT>

SUBCOMMANDS:
    on             Set the current project
    off            Clear the current project status
    restore, rs    Go into the active project folder if it exists
    list, ls       List all projects
EOF
}

WORKSPACE=$HOME/workspace
WORKSPACE_GROUPS=($WORKSPACE/livy $WORKSPACE/writing $WORKSPACE/monara)
CURRENT_PROJECT_FILE=/tmp/active-project

work_on() {
	local project=$1

	if [ -z $1 ]; then
		project=$(work ls | fzf)
	elif [ ! -d $WORKSPACE/$project ]; then
		project=$(work ls | fzf --query $1)
	fi

	if [ -z $project ]; then
		exit 1;
	fi

	echo $project > $CURRENT_PROJECT_FILE
	cd $WORKSPACE/$project

	if [ -s shell.nix ]; then
		nix-shell-active || nix-shell --command zsh
	fi

	if [ -s .workon ]; then
		source .workon
	fi
}

work_off() {
	if [ -s $CURRENT_PROJECT_FILE ]; then
		local project=$(cat $CURRENT_PROJECT_FILE);
		rm $CURRENT_PROJECT_FILE > /dev/null 2>&1

		if [ -s $WORKSPACE/$project/.workoff ]; then
			source $WORKSPACE/$project/.workoff
		fi

		if [ -s $WORKSPACE/$project/shell.nix ]; then
			nix-shell-active && exit
		fi

		cd $HOME
	else
		cd
	fi
}

work_restore() {
	if [ -s $CURRENT_PROJECT_FILE ]; then
		local project=$(cat $CURRENT_PROJECT_FILE);
		cd $WORKSPACE/$project

		if [ -s shell.nix ]; then
			nix-shell-active || nix-shell --command zsh
		fi

		if [ -s .workon ]; then
			source .workon
		fi
	else
		cd
	fi
}

work_status() {
	if [ -s $CURRENT_PROJECT_FILE ]; then
		echo "active: $CURRENT_PROJECT_FILE"
	else
		echo "inactive"
	fi
}

work_list() {
    local escaped=$(echo $WORKSPACE/ | sed 's/\//\\\//g');

    # command ls $WORKSPACE -I `for GROUP in $WORKSPACE_GROUPS; do basename $GROUP; done` | sed "s/$escaped//g"
    command ls $WORKSPACE `for GROUP in $WORKSPACE_GROUPS; do print "\-I $(basename $GROUP)"; done`

    for GROUP in $WORKSPACE_GROUPS
    do
        command ls -d $GROUP/* | sed "s/$escaped//g"
    done
}

work() {
	case "$1" in
		on|'') work_on $2 ;;
		off) work_off $2 ;;
		restore|rs) work_restore ;;
		list|ls) work_list ;;
		status|st) work_status ;;
		*) work_help; return 1 ;;
	esac
}

