#!/bin/sh

# Control the display brightness
# Depends: brightnessctl

help() {
	B=$(tput bold)
	U=$(tput smul)
	N=$(tput sgr0)

	cat << EOF
${B}NAME${N}
	brictl.sh - control the display brightness

${B}SYNOPSIS${N}
	${B}brictl.sh${N} [${U}OPTION${N}] [${U}COMMAND${N}] [<${U}ARG${N}>]

${B}DESCRIPTION${N}
	${B}brictl.sh${N} uses brightnessctl to change the brightness of the system.

	Commands can be truncated, i.e. "${B}brictl.sh s${N}" for "${B}brictl.sh set${N}"
	Arguments need to be of numeric value.

${B}OPTIONS${N}
	${B}-h${N}	Display usage message and exit.

${B}COMMANDS${N}
	${B}i${N} <${U}AMOUNT${N}>, ${B}inc${N} <${U}AMOUNT${N}>
		Increase brightness by ${U}AMOUNT${N}, default of 5.

	${B}d${N} <${U}AMOUNT${N}>, ${B}dec${N} <${U}AMOUNT${N}>
		Decrease brightness by ${U}AMOUNT${N}, default of 5.

	${B}s${N} <${U}LEVEL${N}>, ${B}set${N} <${U}LEVEL${N}>
		Set brightness to ${U}LEVEL${N}, default of 30.

	${B}g${N}, ${B}get${N}
		Get brightness level.
EOF
}

# Exit if no option is provided
[ "$#" -eq 0 ] && help && exit 1

SCRIPT="$(basename "$0")"

# Option handling
while getopts ':h?' opt; do
	case $opt in
		h)
			help
			exit 0
			;;
		\?)
			echo "$SCRIPT: invalid option '$OPTARG'"
			echo "Try '$SCRIPT -h' for more information."
			exit 1
			;;
	esac
done

# Command handling
shift $((OPTIND - 1))
case "$1" in
	i*)
		NUM=${2:-5}
		brightnessctl -q s +"$NUM"%
		;;
	d*)
		NUM=${2:-5}
		brightnessctl -q s "$NUM"%-
		;;
	s*)
		NUM=${2:-30}
		brightnessctl -q s "$NUM"%
		;;
	g*)
		brightnessctl | awk '/%/ {print substr($4, 2, length($4) - 3)}'
		;;
	*)
		echo "$SCRIPT: invalid command '$1'"
		echo "Try '$SCRIPT -h' for more information."
		exit 1
		;;
esac
