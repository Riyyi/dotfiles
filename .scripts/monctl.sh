#!/bin/sh

help() {
	B=$(tput bold)
	U=$(tput smul)
	N=$(tput sgr0)

	cat << EOF
${B}NAME${N}
	monctl.sh - monitor manager

${B}SYNOPSIS${N}
	${B}monctl.sh${N} [${U}OPTION${N}] [${U}ARG(S)${N}]

${B}OPTIONS${N}
	${B}a*${N} ${U}width${N} ${U}height${N} ${U}hertz${N} ${U}name${N}
		Setup single monitor.

${B}ARGS${N}
	width
		Pixel width of the monitor.

	height
		Pixel height of the monitor.

	hertz
		Refresh rate of the monitor.

	name
		Xrandr name of the monitor.
EOF
}

# If no option is provided
[ "$#" -eq 0 ] && help && exit 1

update() {
	sleep 4

	# Reconfigure desktops
	"$HOME"/.scripts/wm/desktops.sh
	sleep 1

	# Restart panel
	"$HOME"/.scripts/panel/lemonbar.sh &

	# Reload wallpaper
	"$HOME"/.scripts/wm/wallpaper.sh
}

auto() {
	[ "$#" != "5" ] && return 1

	# Skip first argument
	shift 1

	# Add mode to primary display
	OUTPUT="$(xrandr -q)"
	if ! echo "$OUTPUT" | grep -Fq "$1x$2_$3.00"; then
		eval xrandr --newmode $(cvt "$1" "$2" "$3" | awk '/Modeline/{ $1=""; print $0 }')
		xrandr --addmode "$4" "$1x$2_$3.00"
	fi

	# Get all connected displays
	CONNECTED="$(xrandr -q | awk '/ connected/{print $1}')"

	# Disable all other displays
	eval xrandr --output "$4" --mode "$1x$2_$3.00" --primary \
		"$(echo "$CONNECTED" | grep -vx "$4" | awk '{print "--output", $1, "--off"}' | tr '\n' ' ')"

	# Post monitor change
	update
}

case "$1" in
	a*)
		auto "$@"
		;;
	*)
		help
		;;
esac
