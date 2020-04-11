#!/bin/sh

# Vim plugin manager
# Depends: git

#----
BASE="$XDG_CONFIG_HOME/vim/pack"
#----
DIR="$BASE/plugins"
LIST="$BASE/list"
START="$DIR/start"
OPT="$DIR/opt"

help() {
	B=$(tput bold)
	U=$(tput smul)
	N=$(tput sgr0)

	cat << EOF
${B}NAME${N}
	vimplugin.sh - vim plugin manager

${B}SYNOPSIS${N}
	${B}vimplugin.sh${N} [${U}OPTION${N}] [${U}COMMAND${N}] [<${U}ARG${N}>...]

${B}OPTIONS${N}
	${B}-h${N}	Display usage message and exit.

${B}COMMANDS${N}
	${B}init${N}
		Creates all the required directories/files.

	${B}install${N} ${U}URL${N} <${U}LOCATION${N}>
		Install a new plugin from a valid GitHub ${U}URL${N}.
		Store this plugin in <${U}LOCATION${N}> (start/opt), default to start.

	${B}list${N} <${U}NUMBER${N}>
		Print all the installed plugins.
		If <${U}NUMBER${N}> is provided, print only the plugin in that position.

	${B}remove${N}
		Remove plugin, pick from selection menu.

	${B}update${N}
		Install/update all plugins stored in the config file.
EOF
}

# Exit if no option is provided
[ "$#" -eq 0 ] && help && exit 1

SCRIPT="$(basename "$0")"

# Option handling
while getopts ':h?' opt; do
	case $opt in
		h)
			help
			exit 0
			;;
		\?)
			echo "$SCRIPT: invalid option '$OPTARG'"
			echo "Try '$SCRIPT -h' for more information."
			exit 1
			;;
	esac
done

init() {
	sudo mkdir -p "$START"
	sudo mkdir -p "$OPT"
	sudo touch "$LIST"
	if [ ! -s "$LIST" ]; then
		# Append to empty file
		sudo sh -c 'echo "start:" >> '"$LIST"
		sudo sh -c 'echo "opt:" >> '"$LIST"
	fi
}

install() {
	# Check if correct git URL
	if [ -z "$(echo "$1" | sed -nr \
		's/^https:\/\/github\.com\/(.*\/.*).git$/\1/p')" ]; then
		echo "$SCRIPT: url invalid: $1"
	else
		# cd to correct directory if called from the CLI
		if [ -z "$2" ]; then
			# Default is directory 'start'
			cd "$START" || exit
		elif [ "$2" = "start" ]; then
			cd "$START" || exit
		elif [ "$2" = "opt" ]; then
			cd "$OPT" || exit
		fi

		REPO="$(basename "$1" .git)"
		if [ -d "$REPO" ]; then
			cd "$REPO" || exit
			sudo git pull --force 1> /dev/null
			cd ".."
			echo "Updated: $REPO"
		else
			sudo git clone "$1" 2> /dev/null

			# Add git URL to config file
			if [ -z "$2" ]; then
				# Append before 'opt:'
				sudo sed -i '/opt:/ i '"$1" "$LIST"
			elif [ "$2" = "start"  ]; then
				# Append before 'opt:'
				sudo sed -i '/opt:/ i '"$1" "$LIST"
			elif [ "$2" = "opt" ]; then
				# Append at the end of the file
				sudo sed -i '$ a '"$1" "$LIST"
			fi

			echo "Installed: $REPO"
		fi
	fi
}

list() {
	if [ -z "$1" ]; then
		echo "Listing all installed plugins.."
	fi

	PLUGIN="$(find "$DIR" -mindepth 2 -maxdepth 2 \
		| sort | awk -F '/' '{ print $(NF-1)"/"$NF }')"
	NUM=0
	for p in $PLUGIN; do
		NUM=$((NUM + 1))

		if [ -z "$1" ]; then
			echo "$NUM) $p"
		elif [ "$NUM" = "$1" ]; then
			echo "$p"
		fi
	done
}

remove() {
	list
	printf "Enter the number to remove: "
	read -r OPTION

	if [ -z "$(echo "$OPTION" | sed -nr 's/^([0-9]+)$/\1/p')" ]; then
		echo "Please select a number"
	else
		TO_REMOVE="$(list "$OPTION")"
		if [ -n "$TO_REMOVE" ]; then
			sudo rm -rf "$DIR/$TO_REMOVE"
			sudo sed -i '/'"$(basename "$TO_REMOVE")"'\.git/d' "$LIST"
			echo "Removed plugin: $TO_REMOVE"
		fi
	fi
}

update() {
	echo "Updating.."

	init

	cd "$START" || exit
	while read -r l; do
		if [ "$l" = "start:"  ]; then
			cd "$START" || exit
		elif [ "$l" = "opt:"  ]; then
			cd "$OPT" || exit
		else
			install "$l" "script"
		fi
	done < "$LIST"
}

# Command handling
shift $((OPTIND - 1))
case "$1" in
	init | install | list | remove | update)
		"$@"
		;;
	*)
		echo "$SCRIPT: invalid command '$1'"
		echo "Try '$SCRIPT -h' for more information."
		exit 1
		;;
esac
