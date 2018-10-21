#!/bin/sh

# Binary prompt script.
# Prompt rofi with label $1 to perform command $2.
#
# Example:
# $ ./prompt.sh "Are you sure you want to shutdown?" "poweroff"

[ "$(printf 'No\nYes' | rofi -dmenu -i -p "$1" -lines 2 \
	-color-window "#524040, #dc7a43, #fff" \
	-color-normal "#524040, #fff, #524040, #fff, #707880")" = "Yes" ] && $2
