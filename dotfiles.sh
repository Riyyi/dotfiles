#!/bin/sh

if [ "$(dirname "$0")" != "." ]; then
	echo "Please run this script from the directory it resides."
	exit
fi

PACKAGE_FILE="./packages"

EXCLUDE="./.git|$0|$PACKAGE_FILE|./README.md|./screenshot.png"
FILES="$(find . -type f | awk -v e="^($EXCLUDE)" '$0 !~ e { print $0 }')"

help() {
	B=$(tput bold)
	N=$(tput sgr0)

	cat << EOF
${B}NAME${N}
	dotfiles - file copy script for the dotfiles repository

${B}SYNOPSIS${N}
	./dotfiles.sh <command> [<args>]

${B}COMMANDS${N}
	${B}list${N}
		Print all files added to the dotfiles directory.

	${B}get <filepath>${N}
		Add file to the dotfiles directory.

	${B}pull${N}
		For each file in the dotfiles directory, copy that file on the system to
		the dotfiles directory.

	${B}push${N}
		For each file in the dotfiles directory, copy that file to its location
		on the system.

	${B}packages [list]${N}
		List all the packages installed on the system.

	${B}packages store${N}
		Store the list of all the installed packages on the system.

	${B}packages install${N}
		Install all the core packages of the stored list.
EOF
}

list() {
	for f in $FILES; do
		echo "${f#??}"
	done
}

get() {
	if [ "$1" = "" ]; then
		echo "Missing argument <filepath>"
		return
	fi

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

pullpush() {
	if [ "$1" = "" ]; then
		return
	fi

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
	pullpush "pull"
}

push() {
	pullpush "push"
}

packages() {
	PACKAGE_LIST="$(pacman -Qqe | sort | grep -vx "$(pacman -Qqg base base-devel | sort)")"

	if [  "$1" = "list" ] || [ "$1" = "" ]; then
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

if type "$1" 2> /dev/null | grep -q "function"; then
	"$@"
else
	help
fi

# @Todo:
# get function to support symlinks
# push function to push just one file
