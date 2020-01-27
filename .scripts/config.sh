#!/bin/sh

# Script that searches config files and scripts

# User-config---------------------------

# Define editor to open file in and if it should fork
EDITOR="emacsclient"
BACKGROUND="&"

# List of directories to exclude
EXCLUDES="
Code
Documents
dotfiles
Downloads
pictures
.cache
.clangd
.config/chromium
.config/ElectronChrome
.config/vim/pack/plugins
.electron-gyp
.emacs.d/elpa
.gradle
.jd
.local/share
.mozilla/firefox/rick.profile/storage
.node-gyp
.npm
.nvm
.wine
"

# List of files to include
INCLUDES="
Documents/vm/backup/backup.sh
Documents/vm/commands.org
dotfiles/dotfiles.sh
"

# --------------------------------------

# Execute relative to $HOME
cd "$HOME" || exit 1

# Generate exclude string to use with find
for EXCLUDE in $EXCLUDES; do
	EXCLUDE_STRING="$EXCLUDE_STRING -path ./$EXCLUDE -o"
done
EXCLUDE_STRING=${EXCLUDE_STRING%???}

# Find the files
FILES="$( (find . \( $EXCLUDE_STRING \) -prune -o -printf '%P\n'; echo "$INCLUDES") \
	| grep -vx "" | sort)"

# If no name provided
if [ -z "$1" ]; then
	SELECTED="$(printf "%s" "$FILES" | rofi -dmenu -p "Select file to edit")"

# Else try to search for that file
else
	SELECTED="$(printf "%s" "$FILES" | grep "$1" | head -n 1)"
fi

# Exit if nothing selected
if [ -z "$SELECTED" ]; then
	exit
fi

# Start editor with selected file
if [ -z "$BACKGROUND" ]; then
	$EDITOR "$SELECTED"
else
	$EDITOR "$SELECTED" > /dev/null 2>&1 &
fi
