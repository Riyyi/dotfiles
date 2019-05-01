#!/bin/sh

ICON=""
WIRED="${1:-eth0}"

COLOR="-"
if [ ! -d /sys/class/net/${WIRED} ] ||
	[ "$(cat /sys/class/net/$WIRED/operstate)" = 'down' ] ||
	[ "$(ip a | grep $WIRED | awk '/inet / {print $2}')" = "" ]; then
	COLOR="$COLOR7"
else
	COLOR="$COLOR15"
fi

printf "%s\n" "iface%{F$COLOR}$ICON%{F-}"
