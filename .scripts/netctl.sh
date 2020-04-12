#!/bin/sh

# Control the network interfaces
# Depends: iproute2, netctl

WIRELESS="wlan0"
ETHERNET="eth0"
PROFILE="eth0-dhcp"

help() {
	B=$(tput bold)
	U=$(tput smul)
	N=$(tput sgr0)

	cat << EOF
${B}NAME${N}
	netctl.sh - control the network interfaces

${B}SYNOPSIS${N}
	${B}netctl.sh${N} [${U}OPTION${N}] [${U}COMMAND${N}]

${B}OPTIONS${N}
	${B}-h${N}	Display usage message and exit.

	${B}-e${N} [${U}STATE${N}]
		Perform action on ethernet.

	${B}-w${N} [${U}STATE${N}]
		Perform action on wireless.

${B}COMMANDS${N}
	1, on
		Enable selected option.

	0, off
		Disable selected option.
EOF
}

# Exit if no option is provided
[ "$#" -eq 0 ] && help && exit 1

SCRIPT="$(basename "$0")"

# Option handling
while getopts ':h?e:w:' opt; do
	case $opt in
		h)
			help
			exit 0
			;;
		e)
			dev="ethernet"
			;;
		w)
			dev="wireless"
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

ethernet() {
	sudo netctl stop $PROFILE
	sudo ip link set $ETHERNET down

	if [ "$1" -eq 1 ]; then
		sudo netctl start $PROFILE
	fi
}

wireless() {
	if [ "$1" -eq 1 ]; then
		sudo ip link set $WIRELESS up
	else
		sudo ip link set $WIRELESS down
	fi
}

# Command handling
[ $OPTIND -ge 2 ] && shift $((OPTIND - 2))
case "$1" in
	1 | on)
		[ "$dev" = "ethernet" ] && ethernet 1 || wireless 1
		;;
	0 | off)
		[ "$dev" = "ethernet" ] && ethernet 0 || wireless 0
		;;
	*)
		echo "$SCRIPT: invalid command '$1'"
		echo "Try '$SCRIPT -h' for more information."
		exit 1
		;;
esac
