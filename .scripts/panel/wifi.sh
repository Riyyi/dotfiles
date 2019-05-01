#!/bin/sh

ICON=""
WIRELESS="${1:-wlan0}"

if [ "$NOTIFY" = "1" ]; then
	SSID="$(iw dev $WIRELESS link | awk '/SSID/ { print $2 }')"
	IP="$(ip a show $WIRELESS \
		| awk '/inet / { print substr($2, 0, length($2) - 3) }')"

	notify-send -r 3 "Wifi" "\
Interface: $WIRELESS
SSID:      $SSID
IP:        $IP"
else
	COLOR="-"
	if [ ! -d /sys/class/net/${WIRELESS}/wireless ] ||
		[ "$(cat /sys/class/net/$WIRELESS/operstate)" = 'down' ]; then

		COLOR="$COLOR7"
		DISPLAY="$ICON"
	else
		QUALITY=$(grep $WIRELESS /proc/net/wireless | \
			awk '{ print int($3 * 100 / 70) }')

		COLOR="$COLOR15"
		DISPLAY="$ICON $QUALITY%"
	fi

	printf "%s\n" "wifi%{F$COLOR}%{A:NOTIFY=1 $0:}$DISPLAY%{A}%{F-}"
fi
