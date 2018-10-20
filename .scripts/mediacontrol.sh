#!/bin/sh

# General audio management

RELOAD="pkill -RTMIN+1 i3blocks"

[ -z "$2" ] && NUM="2" || NUM="$2"

case "$1" in
	u*)     pulsemixer --change-volume +"$NUM" ; $RELOAD ;;
	d*)     pulsemixer --change-volume -"$NUM" ; $RELOAD ;;
	s*)     pulsemixer --set-volume "$NUM" ; $RELOAD ;;
	t*)     pulsemixer --toggle-mute ; $RELOAD ;;
	m*)     pulsemixer --mute ; $RELOAD ;;
	u*)     pulsemixer --unmute ; $RELOAD ;;
	getv*)  pulsemixer --get-volume | awk '{print $1}' ;;
	getm*)  pulsemixer --get-mute ;;
esac
