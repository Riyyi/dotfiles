#!/bin/sh

# Script that searches config files and scripts

# Define editor to open file in
EDITOR="$EDITOR"

# List of locations to search
LOCATIONS="$HOME/.config $HOME/.scripts"

# Find the files
FILES="$(find -L $LOCATIONS -maxdepth 3 -type f | sort)"
HOME="$(find -L $HOME -maxdepth 1 -type f | sort)"

# If no name provided
if [ -z "$1" ]; then
	SELECTED="$(printf "%s\n%s" "$FILES" "$HOME" \
		| uniq | rofi -dmenu -p "Select file to edit")"

# Else try to search for that file
else
	SELECTED="$(printf "%s\n%s" "$FILES" "$HOME" | grep "$1" | head -n 1)"
fi

# Start editor with selected file (if any)
[ -n "$SELECTED" ] && $EDITOR "$SELECTED"
