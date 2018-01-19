#!/bin/sh

WIRELESS="${BLOCK_INSTANCE:-wlan0}"

if [ ! -d /sys/class/net/${WIRELESS}/wireless ] ||
	[ "$(cat /sys/class/net/$WIRELESS/operstate)" = 'down' ]; then
	echo ""
	echo ""
	echo "#676E7D"
else
	SSID=$(iw dev $WIRELESS link | awk '/SSID/ { print $2 }')
	QUALITY=$(grep $WIRELESS /proc/net/wireless | awk '{ print int($3 * 100 / 70) }')
	
	echo "$SSID  $QUALITY%"
	echo "$ $QUALITY%"
	echo "#FFFFFF"
fi
