#!/bin/sh

WIRELESS="wlan0"
WIRED="eth0"
PROFILE="eth0-dhcp"

if [ -z "$1" ] || [ -z "$2" ]; then
	exit;
fi

# Enable/disable wireless
if [ "$1" = "w" ] || [ "$1" = "$WIRELESS" ]; then
	if [ "$2" = "1" ]; then
		sudo ip link set $WIRELESS up
	else
		sudo ip link set $WIRELESS down
	fi
fi

# Enable/disable wired
if [ "$1" = "e" ] || [ "$1" = "$WIRED" ]; then

	sudo netctl stop $PROFILE
	sudo ip link set $WIRED down

	if [ "$2" = "1" ]; then
		sudo netctl start $PROFILE
	fi
fi
