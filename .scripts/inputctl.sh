#!/bin/sh

# Control the input devices
# Depends: xinput, setxkbmap

help() {
	B=$(tput bold)
	U=$(tput smul)
	N=$(tput sgr0)

	cat << EOF
${B}NAME${N}
	inputctl.sh - control the input devices

${B}SYNOPSIS${N}
	${B}inputctl.sh${N} ${U}OPTION${N} [${U}ARG${N}]

${B}OPTIONS${N}
	${B}-h${N}	Display usage message and exit.

	${B}-d${N} ${U}STATE${N}
		Set touchpad state, possible values: toggle, on/off, 1/0.

	${B}-k${N} ${U}STATE${N}
		Set keyboard state, possible values: toggle, on/off, 1/0.

	${B}-s${N} ${U}STATE${N}
		Set touchscreen state, possible values: toggle, on/off, 1/0.
EOF
}

setxkbmap -option caps:swapescape

# Exit if no option is provided
[ "$#" -eq 0 ] && help && exit 1

# Set required X variables
export DISPLAY=:0
export XAUTHORITY="$XDG_DATA_HOME/xorg/Xauthority"

# Option handling
SCRIPT="$(basename "$0")"
while getopts ':h?d:k:s:' opt; do
	case $opt in
		h)
			help
			exit 0
			;;
		d)
			OPTION="d"
			ARG="$OPTARG"
			DEV="SYNA3602:00 0911:5288 Touchpad"
			;;
		k)
			OPTION="k"
			ARG="$OPTARG"
			;;
		s)
			OPTION="s"
			ARG="$OPTARG"
			DEV="pointer:04F3200A:00 04F3:2373"
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
	STATUS=$(xinput --list-props "$2" | awk '/Device Enabled/ { print $4 }')
	[ "$STATUS" -eq 1 ] && xinput --disable "$2" || xinput --enable "$2"
}

device() {
	case "$1" in
		1 | on)
			xinput --enable "$2"
			;;
		0 | off)
			xinput --disable "$2"
			;;
		t*)
			toggle_device "$2"
			;;
		*)
			echo "$SCRIPT: invalid argument '$1'"
			echo "Try '$SCRIPT -h' for more information."
			exit 1
			;;
	esac
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

# Option execution
# if [ "$DEV" = "Keyboard" ]; then
# 	keyboard "$ARG"
# else
case "$OPTION" in
	d | s)
		device "$ARG" "$DEV"
		;;
	k)
		keyboard "$ARG"
		;;
esac

# Useful input diagnostics packages:
# - xinput
# - xev
# - evtest
#
# - setxkbmap -query
# - xmodmap -pke
