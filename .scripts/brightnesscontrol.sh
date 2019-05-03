#!/bin/sh

# General brightness management

RELOAD="$HOME/.scripts/panel/brightness.sh"

[ -z "$2" ] && NUM="5" || NUM="$2"

help() {
	B=$(tput bold)
	N=$(tput sgr0)

	cat << EOF
${B}NAME${N}
	brightnesscontrol - control the volume of the system

${B}SYNOPSIS${N}
	./brightnesscontrol.sh <command> [<arg1>]

${B}DESCRIPTION${N}
	Commands can be truncated, i.e.
	\`${B}brightnesscontrol.sh s${N}\` for \`${B}brightnesscontrol.sh set${N}\`

	Arguments need to be of numeric value.

${B}COMMANDS${N}
	${B}u*, up <amount>${N}

	${B}d*, down <amount>${N}

	${B}s*, set <volume>${N}

	${B}g*, getbrightness${N}
EOF
}

case "$1" in
	u*) brightnessctl -q s +"$NUM"% ; $RELOAD ;;
	d*) brightnessctl -q s "$NUM"%- ; $RELOAD ;;
	s*) brightnessctl -q s "$NUM"% ; $RELOAD ;;
	g*) brightnessctl | awk '/%/ {print substr($4, 2, length($4) - 3)}' ;;
	*)  help ;;
esac
