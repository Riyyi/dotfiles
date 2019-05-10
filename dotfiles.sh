#!/bin/sh

# User-config---------------------------

PACKAGE_FILE="packages"
EXCLUDE_FILES="${0#??}|$PACKAGE_FILE|.*.md$|.*README.org$|.git|screenshot.png"

# --------------------------------------

if [ "$(dirname "$0")" != "." ]; then
	echo "Please run this script from the directory it resides."
	exit
fi

FILES="$(find . -type f \
	| awk -v e="^($EXCLUDE_FILES)" 'substr($0, 3) !~ e { print $0 }')"

help() {
	B=$(tput bold)
	U=$(tput smul)
	N=$(tput sgr0)

	cat << EOF
${B}NAME${N}
	dotfiles - file copy script for the dotfiles repository

${B}SYNOPSIS${N}
	${B}./dotfiles.sh${N} ${U}OPTION${N} [${U}ARG${N}]

${B}OPTIONS${N}
	${B}-a${N} ${U}FILE${N}, ${B}--add${N} ${U}FILE${N}
		Add file to the dotfiles directory.

	${B}-f, --files${N}
		Display all files added to the dotfiles directory.

	${B}-h, --help${N}
		Display usage message and exit.

	${B}-p${N} [${U}ARG${N}], ${B}--packages${N} [${U}ARG${N}]
		${B}install${N}
		Install all core packages of the stored list.

		${B}list${N} (default)
		Display all packages installed on the system.

		${B}store${N}
		Stores a list of all installed packages on the system.

	${B}-l, --pull${N}
		Pull each added file from the system to the dotfiles directory.

	${B}-s, --push${N}
		Push each added file to its location on the system.
EOF
}

files() {
	for f in $FILES; do
		echo "${f#??}"
	done
}

add() {
	[ "$1" = "" ] && return 1

	FILE=$(readlink -f "$1")
	FILE_CUT_HOME="$(echo "$FILE" | sed -nr 's/^\/home\/'"$USER"'\/(.*)$/\1/p')"

	# /home/<user>/
	if [ -n "$FILE_CUT_HOME" ]; then
		mkdir -p "$(pwd)/$(dirname "$FILE_CUT_HOME")"
		cp "$FILE" "$(pwd)/$FILE_CUT_HOME"
	# /
	else
		mkdir -p "$(pwd)/$(dirname "$FILE")"
		cp "$FILE" "$(pwd)/$FILE"
	fi
}

pull_push() {
	[ "$1" = "" ] && return 1

	for f in $FILES; do
		# Remove the first character (.) from the string
		f=${f#?}
		# Resolved symbolic link
		fr=$(readlink -f "$f")

		# The filepath starts with '/boot/', '/etc/', '/usr/share/'
		if [ -n "$(echo "$fr" | sed -nr 's/^(\/(boot|etc|usr\/share)\/).*$/\1/p')" ]; then
			if [ "$1" = "pull" ]; then
				sudo cp "$fr" "$(pwd)/$fr"
			elif [ "$1" = "push" ]; then
				sudo cp "$(pwd)/$fr" "$fr"
			fi
		else
			if [ "$1" = "pull" ]; then
				# cp /home/<user>/<file> /home/<user>/[<some dir>/]dotfiles/<file>
				cp "$HOME$f" "$(pwd)/$f"
			elif [ "$1" = "push" ]; then
				cp "$(pwd)/$f" "$HOME$f"
			fi
		fi
	done
}

pull() {
	pull_push "pull"
}

push() {
	pull_push "push"
}

packages() {
	PACKAGE_LIST="$(pacman -Qqe | sort | grep -vx "$(pacman -Qqg base base-devel | sort)")"

	if [ "$1" = "list" ] || [ "$1" = "" ]; then
		echo "$PACKAGE_LIST"
	elif [ "$1" = "store" ]; then
		if [ ! -s $PACKAGE_FILE ]; then
			touch "$PACKAGE_FILE"
		else
			truncate -s 0 "$PACKAGE_FILE"
		fi
		echo "$PACKAGE_LIST" > "$PACKAGE_FILE"
	elif [ "$1" = "install" ]; then
		# Install core packages
		sudo pacman -S --needed --noconfirm "$(cat "$PACKAGE_FILE")"
		# For AUR packages, run: <helper> -S - < packages
	fi
}

# --------------------------------------

SCRIPT="$(basename "$0")"

# $1 = -option, $2 message
option_wrong() {
	[ -z "$1" ] || [ -z "$2" ] && return 1

	echo "$SCRIPT: $2 '$1'" >&2
	echo "Try '$SCRIPT -h' or '$SCRIPT --help' for more information." >&2
	exit 1
}

# Get all the -options
OPTIONS=$(echo "$@" | awk '{
	for(i=1; i<=NF; i++) { if ($i ~ /^-/) printf "%s ", $i }
}' | wc -w)

# Check if -options are valid
if [ "$OPTIONS" -gt 1 ]; then
	LAST="$(echo "$@" | cut -d ' ' -f $#)"
	option_wrong "$LAST" "option too many"
elif [ "$#" -gt 2 ]; then
	LAST="$(echo "$@" | cut -d ' ' -f $#)"
	option_wrong "$LAST" "argument too many"
fi

OPT="$(echo "$* " | cut -d ' ' -f 1)"
ARG="$(echo "$* " | cut -d ' ' -f 2)"

# Parse -options and call functions
case $OPT in
	-a | --add)
		add "$ARG" || option_wrong "$OPT" 'option requires an argument'
		;;
	-f | --files)
		files
		;;
	-h | --help)
		help
		;;
	-p | --packages)
		packages "$ARG"
		;;
	-l | --pull)
		pull
		;;
	-s | --push)
		push
		;;
	*)
		option_wrong "$OPT" 'invalid option'
		;;
esac

# @Todo:
# get function to support symlinks
# get function to add entire new directory including contents
# push function to push just one file
