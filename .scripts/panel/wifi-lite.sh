#!/bin/sh

WIRELESS="${1:-wlan0}"

SSID="$(iw dev "$WIRELESS" link | awk '/SSID/ { print $2 }')"
IP="$(ip a show "$WIRELESS" \
		| awk '/inet / { print substr($2, 0, length($2) - 3) }')"

notify-send -r 3 "Wifi" "\
Interface: $WIRELESS
SSID:      $SSID
IP:        $IP"
