#!/bin/sh

help() {
	B=$(tput bold)
	U=$(tput smul)
	N=$(tput sgr0)

	cat << EOF
${B}NAME${N}
	pidctl.sh - Daemon and program starter

${B}SYNOPSIS${N}
	${B}pidctl.sh${N} [${U}OPTION${N}]

${B}OPTIONS${N}
	${B}-d${N}
		Start daemons, run this from xinitrc.

	${B}-p${N}
		Program startup, run this from WM config.
EOF
}

start() {
	[ -z "$1" ] && return 1

	IS_RUNNING=$(pidof -x "$(echo "$@" | cut -d ' ' -f 1)")
	[ -z "$IS_RUNNING" ] && $@ &
}

daemon() {
	start dunst
	start redshift

	start emacs --daemon
	start thunar --daemon

	start sxhkd "$XDG_CONFIG_HOME/sxhkd/$WM"
	start xss-lock -- "$HOME"/.scripts/wm/lock.sh

	[ "$WM" = "bspwm" ] && start stalonetray
}

program() {
	# Application startup always

	"$HOME"/.scripts/wm/wallpaper.sh &
	"$HOME"/.scripts/panel/lemonbar.sh &

	# Application startup once

	# Firefox
	start firefox

	# Thunar
	[ "$(xdo id -a Thunar | wc -l)" -lt 4 ] && thunar &

	# URxvt
	if [ "$WM" = "bspwm" ]; then
		# Start urxvt on desktop 3
		$(bspc rule -a URxvt desktop="^$WS3" \
			  && start urxvt \
			  && sleep 3 \
			  && bspc rule -r URxvt desktop) &

	elif [ "$WM" = "i3" ]; then
		start urxvt
	fi
}

# If no option is provided
[ "$#" -eq 0 ] && help && exit

SCRIPT="$(basename "$0")"
# Option handling
while getopts 'hdp' opt; do
	case $opt in
		h)
			help
			exit
			;;
		d)
			daemon
			;;
		p)
			program
			;;
		\?)
			echo "Try '$SCRIPT -h' for more information."
			exit 1
			;;
	esac
done
