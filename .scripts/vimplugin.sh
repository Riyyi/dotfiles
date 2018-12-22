#!/bin/sh

#----
BASE="$XDG_CONFIG_HOME/vim/pack"
#----
DIR="$BASE/plugins"
LIST="$BASE/list"
START="$DIR/start"
OPT="$DIR/opt"

help() {
	B=$(tput bold)
	N=$(tput sgr0)

	cat << EOF
${B}NAME${N}
	vimplugin - vim plugin manager

${B}SYNOPSIS${N}
	./vimplugin.sh <command> [<arg1> <arg2>]

${B}COMMANDS${N}
	${B}init${N}
		Creates all the required directories/files.

	${B}install <url> ["start"/"opt"]${N}
		Install a new plugin, <url> need to be a valid GitHub URL,
		start/opt to install the plugin in the start/opt directory
		(default: start).

	${B}list <number>${N}
		Prints all the installed plugins,
		<number> prints just the plugin in that position.
	${B}remove${N}
		Remove plugin, script will prompt a selection menu.
	${B}update${N}
		Installs/updates all plugins from the config file.
EOF
}

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

list() {
	if [ -z "$1" ]; then
		echo "Listing all installed plugins.."
	fi

	PLUGIN="$(find "$DIR" -mindepth 2 -maxdepth 2 \
		| sort | awk -F '/' '{ print $(NF-1)"/"$NF }')"
	NUM=0
	for p in $PLUGIN; do
		NUM=$(( NUM + 1 ))

		if [ -z "$1" ]; then
			echo "$NUM) $p"
		elif [ "$NUM" = "$1" ]; then
			echo "$p"
		fi
	done
}

install() {
	# Check if correct git URL
	if [ -z "$(echo "$1" | sed -nr \
		's/^https:\/\/github\.com\/(.*\/.*).git$/\1/p')" ]; then
		echo "script: url invalid: $1"
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

remove() {
	list
	echo -n "Enter the number to remove: "
	read -r OPTION

	if [ -z "$(echo "$OPTION" | sed -nr 's/^([0-9]+)$/\1/p')" ]; then
		echo "Please select a number"
	else
		TO_REMOVE="$(list "$OPTION")"
		if [ -n "$TO_REMOVE" ]; then
			sudo rm -rf "$DIR/$TO_REMOVE"
			sudo sed -i '/'"$(basename "$TO_REMOVE")"'\.git/d' "$LIST"
		fi
	fi
}

if type "$1" 2> /dev/null | grep -q "function"; then
	"$@"
else
	help
fi

