#!/bin/sh

# General audio management

RELOAD="pkill -RTMIN+1 i3blocks"

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

	${B}u*, unmute${N}

	${B}getv*, getvolume${N}

	${B}getm*, getmute${N}
EOF
}

case "$1" in
	u*)     pulsemixer --change-volume +"$NUM" ; $RELOAD ;;
	d*)     pulsemixer --change-volume -"$NUM" ; $RELOAD ;;
	s*)     pulsemixer --set-volume "$NUM" ; $RELOAD ;;
	t*)     pulsemixer --toggle-mute ; $RELOAD ;;
	m*)     pulsemixer --mute ; $RELOAD ;;
	u*)     pulsemixer --unmute ; $RELOAD ;;
	getv*)  pulsemixer --get-volume | awk '{print $1}' ;;
	getm*)  pulsemixer --get-mute ;;
	*) help ;;
esac
