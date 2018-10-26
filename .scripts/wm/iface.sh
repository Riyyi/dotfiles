#!/bin/sh

WIRED="${BLOCK_INSTANCE:-eth0}"

echo ""
echo ""
if [ ! -d /sys/class/net/${WIRED} ] ||
	[ "$(cat /sys/class/net/$WIRED/operstate)" = 'down' ] ||
	[ "$(ip a | grep $WIRED | awk '/inet / {print $2}')" = "" ]; then
	echo "$COLOR7"
else
	echo "$COLOR15"
fi
