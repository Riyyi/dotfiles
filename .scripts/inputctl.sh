#!/bin/sh

help() {
	B=$(tput bold)
	U=$(tput smul)
	N=$(tput sgr0)

	cat << EOF
${B}NAME${N}
	inputctl.sh - input manager

${B}SYNOPSIS${N}
	${B}inputctl.sh${N} [${U}OPTION${N}] [${U}ARG${N}]

${B}OPTIONS${N}
	${B}-h${N}
		Display usage message and exit.

	${B}-d${N} [${U}ARG${N}]
		Perform action on touchpad.

	${B}-k${N} [${U}ARG${N}]
		Perform action on keyboard.

	${B}-s${N} [${U}ARG${N}]
		Perform action on touchscreen.

${B}ARGS${N}
	toggle
		Toggle input device / keyboard customizations.

	on
		Enable input device / keyboard customizations.

	off
		Disable input device / keyboard customizations.
EOF
}

# If no option is provided
[ "$#" -eq 0 ] && help && exit

# Set required X variables
export DISPLAY=:0
export XAUTHORITY="$XDG_DATA_HOME/xorg/Xauthority"

SCRIPT="$(basename "$0")"
# Option handling
while getopts ':h?d:k:s:' opt; do
	case $opt in
		h)
			help
			exit
			;;
		d)
			dev="SYNA3602:00 0911:5288 Touchpad"
			;;
		k)
			dev="Keyboard"
			;;
		s)
			dev="pointer:04F3200A:00 04F3:2373"
			;;
		:)
			echo "$SCRIPT: option requires an argument '$OPTARG'"
			echo "Try '$SCRIPT -h' for more information."
			exit 1
			;;
		\?)
			echo "$SCRIPT: invalid option '$OPTARG'"
			echo "Try '$SCRIPT -h' for more information."
			exit 1
			;;
	esac
done

toggle_device() {
	STATUS=$(xinput --list-props "$dev" | awk '/Device Enabled/ { print $4 }')
	[ "$STATUS" -eq 1 ] && xinput --disable "$dev" || xinput --enable "$dev"
}

keyboard() {
	[ -z "$1" ] && return 1

	if [ "$1" = "toggle" ]; then
		if setxkbmap -query | grep -q options; then
			keyboard "off"
		else
			keyboard "on"
		fi

	elif [ "$1" = "on" ]; then
		# Swap caps lock with escape
		setxkbmap -option caps:swapescape

		# Swap left crtl with left alt
		# setxkbmap -option ctrl:swap_lalt_lctl

		# Set touchpad toggle keyboard symbol
		# xmodmap -e "keycode 93 = XF86iTouch"

	elif [ "$1" = "off" ]; then
		# Clear all key mappings
		setxkbmap -option ''
	fi
}

shift $((OPTIND - 2))
# Command handling
if [ "$dev" = "Keyboard" ]; then
	keyboard "$1"
else
	case "$1" in
		toggle)
			toggle_device
			;;
		on)
			xinput --enable "$dev"
			;;
		off)
			xinput --disable "$dev"
			;;
	esac
fi

# Useful input diagnostics packages:
# - xinput
# - xev
# - evtest
#
# - setxkbmap -query
# - xmodmap -pke
#
# Kernel 5.1.4 works properly on my hardware with the i2c_hid module
