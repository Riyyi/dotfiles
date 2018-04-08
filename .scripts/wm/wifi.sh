#!/bin/sh

WIRELESS="${BLOCK_INSTANCE:-wlan0}"

if [ ! -d /sys/class/net/${WIRELESS}/wireless ] ||
	[ "$(cat /sys/class/net/$WIRELESS/operstate)" = 'down' ]; then
	echo ""
	echo ""
	echo "#676E7D"
else
	SSID="$(iw dev $WIRELESS link | awk '/SSID/ { print $2 }')"
	QUALITY=$(grep $WIRELESS /proc/net/wireless | \
		awk '{ print int($3 * 100 / 70) }')

	echo " $QUALITY%"
	echo " $QUALITY%"
	echo "#FFFFFF"
fi

notify() {
	notify-send -r 3 "Wifi" "\
Interface: $WIRELESS
SSID:      $SSID
IP:        $(ip a show $WIRELESS | \
	awk '/inet / { print substr($2, 0, length($2) - 3) }')"
}

case $BLOCK_BUTTON in
	1) notify ;;
esac
