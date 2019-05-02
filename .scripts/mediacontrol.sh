#!/bin/sh

# General audio management

RELOAD="$HOME/.scripts/panel/volume.sh"

[ -z "$2" ] && NUM="2" || NUM="$2"

help() {
	B=$(tput bold)
	N=$(tput sgr0)

	cat << EOF
${B}NAME${N}
	mediacontrol - control the volume of the system

${B}SYNOPSIS${N}
	./mediacontrol.sh <command> [<arg1>]

${B}DESCRIPTION${N}
	Commands can be truncated, i.e.
	\`${B}mediacontrol.sh t${N}\` for \`${B}mediacontrol.sh toggle${N}\`

	Arguments need to be of numeric value.

${B}COMMANDS${N}
	${B}u*, up <amount>${N}

	${B}d*, down <amount>${N}

	${B}s*, set <volume>${N}

	${B}t*, toggle${N}

	${B}m*, mute${N}

	${B}n*, notmute${N}

	${B}getv*, getvolume${N}

	${B}getm*, getmute${N}
EOF
}

case "$1" in
	u*)    pamixer --increase "$NUM" ; $RELOAD ;;
	d*)    pamixer --decrease "$NUM" ; $RELOAD ;;
	s*)    pamixer --set-volume "$NUM" ; $RELOAD ;;
	t*)    [ "$(pamixer --get-mute)" = "false" ] && \
	           pamixer --mute || pamixer --unmute ; $RELOAD ;;
	m*)    pamixer --mute ; $RELOAD ;;
	n*)    pamixer --unmute ; $RELOAD ;;
	getv*) pamixer --get-volume ;;
	getm*) pamixer --get-mute ;;
	*)     help ;;
esac
