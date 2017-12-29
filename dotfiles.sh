#!/bin/bash

if [ "$(dirname $0)" != "." ]; then
	echo "Please run this script from the directory it resides."
	exit
fi

FILES=$(find . \( -path ./.git -o \
		  -path ./dotfiles.sh \) -prune -o -type f -print)

if [ "$1" == "help" ] || [ "$1" == "" ]; then
	BOLD=$(tput bold)
	NORMAL=$(tput sgr0)
	
	echo "${BOLD}NAME${NORMAL}"
	echo "        dotfiles - file copy script for this repository"
	echo ""
	echo "${BOLD}SYNOPSIS${NORMAL}"
       	echo "        ./dotfiles.sh <command> [<args>]"
	echo ""	
	echo "${BOLD}COMMANDS${NORMAL}"
	echo "        list"
	echo "                Prints all the dotfiles added to this directory."
	echo ""
	echo "        get <filepath>"
	echo "                Add file to this directory."
	echo ""
	echo "        pull"
	echo "                For each file in this directory, copy that file \
on the system to this directory."
	echo ""
	echo "        push"
	echo "                For each file in this directory, copy that file \
to its location on the system."

elif [ "$1" == "list" ]; then
	for f in $FILES; do
		echo $f
	done

elif [ "$1" == "get" ] && [ "$2" != "" ]; then
	FILE=$(readlink -f $2)
	mkdir -p $(pwd)/$(dirname $FILE)
	cp $FILE $(pwd)/$FILE

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

else
	echo "./dotfiles.sh: '$1' is not a dotfiles command. \
See './dotfiles.sh help'."

fi

