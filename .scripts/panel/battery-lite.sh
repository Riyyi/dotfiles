#!/bin/sh

BAT_INFO=${BLOCK_INSTANCE:-"$(acpi -b | grep ': [^Unknown]')"}

CAPACITY=$(echo "$BAT_INFO" | awk '{ print int($4) }')

if [ "$(echo "$BAT_INFO" | awk '{ print $6 }')" = "remaining" ]; then
	if [ "$CAPACITY" -lt "20" ]; then
		notify-send -u critical -r 1 -t 30000 "Battery critically low!"
	fi
elif [ "$CAPACITY" -ge "95" ]; then
	notify-send -u low -r 1 "You should probably unplug."
fi
