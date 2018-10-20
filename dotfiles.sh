#!/bin/bash

if [ "$(dirname $0)" != "." ]; then
	echo "Please run this script from the directory it resides."
	exit
fi

PACKAGE_FILE="./packages"

FILES=$(find . \( -path ./.git -o \
		  -path ./dotfiles.sh -o \
		  -path $PACKAGE_FILE -o \
		  -path ./README.md -o \
		  -path ./screenshot.png \) -prune -o -type f -print)

if [ "$1" == "help" ] || [ "$1" == "" ]; then
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
		Install all the core packages of the list.
EOF

elif [ "$1" == "list" ]; then
	for f in $FILES; do
		echo $f
	done

elif [ "$1" == "get" ] && [ "$2" != "" ]; then
	FILE=$(readlink -f $2)
	FILE_CUT_HOME="$(echo $FILE | sed -nr 's/^\/home\/'$USER'\/(.*)$/\1/p')"

	# /home/<user>/
	if [ -n "$FILE_CUT_HOME" ]; then
		mkdir -p $(pwd)/$(dirname $FILE_CUT_HOME)
		cp $FILE $(pwd)/$FILE_CUT_HOME
	# /
	else
		mkdir -p $(pwd)/$(dirname $FILE)
		cp $FILE $(pwd)/$FILE
	fi

elif [ "$1" == "pull" ] || [ "$1" == "push" ]; then
	for f in $FILES; do
		# Remove the first character (.) from the string
		f=${f:1}
		# Resolved symbolic link
		fr=$(readlink -f $f)

		# The filepath starts with '/boot/', '/etc/', '/usr/share/'
		if [ -n "$(echo $fr | sed -nr 's/^(\/(boot|etc|usr\/share)\/).*$/\1/p')" ]; then
			if [ "$1" == "pull" ]; then
				sudo cp $fr $(pwd)/$fr
			else
				sudo cp $(pwd)/$fr $fr
			fi
		else
			if [ "$1" == "pull" ]; then
				# cp /home/<user>/<file> /home/<user>/[<some dir>/]dotfiles/<file>
				cp $HOME$f $(pwd)/$f
			else
				cp $(pwd)/$f $HOME$f
			fi
		fi
	done

elif [ "$1" == "packages" ]; then
	PACKAGE_LIST=$(comm -23 <(pacman -Qeq | sort) <(pacman -Qgq base base-devel | sort))

	if [  "$2" == "list" ] || [ "$2" == "" ]; then
		echo "$PACKAGE_LIST"
	elif [ "$2" == "store" ]; then
		if [ ! -s $PACKAGE_FILE ]; then
			touch "$PACKAGE_FILE"
		else
			truncate -s 0 "$PACKAGE_FILE"
		fi
		echo "$PACKAGE_LIST" > "$PACKAGE_FILE"
	elif [ "$2" == "install" ]; then
		# Install core packages
		sudo pacman -S --needed $(comm -12 <(pacman -Slq | sort) <(sort $PACKAGE_FILE))
		# For AUR packages, run: <helper> -S - < packages
	fi
else
	echo "./dotfiles.sh: '$1' is not a dotfiles command. \
See './dotfiles.sh help'."

fi

# @Todo:
# get function to support symlinks
# push function to push just one file
