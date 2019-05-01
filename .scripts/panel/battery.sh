#!/bin/sh

BAT_INFO=${BLOCK_INSTANCE:-"$(acpi -b | grep ': [^Unknown]')"}

CAPACITY=$(echo $BAT_INFO | awk '{ print int($4) }')
TIME="$(echo $BAT_INFO | awk '{ print substr($5, 0, length($5) - 3) }')"
CHARACTER=":"

if [ "$TIME" != "" ] && [ "${TIME#*$CHARACTER}" != "$TIME" ]; then
	TIME=" ($TIME)"
else
	TIME=""
fi

COLOR="-"
if [ "$(echo $BAT_INFO | awk '{ print $6 }')" = "remaining" ]; then
	if [ "$CAPACITY" -ge "80" ]; then
		ICON=""
	elif [ "$CAPACITY" -ge "60" ]; then
		ICON=""
	elif [ "$CAPACITY" -ge "40" ]; then
		ICON=""
	elif [ "$CAPACITY" -ge "20" ]; then
		ICON=""
	else
		ICON=""
		COLOR="$COLOR9"

		notify-send -u critical -r 1 -t 30000 "Battery critically low!"
	fi
else
	if [ "$CAPACITY" -ge "95" ]; then
		notify-send -u low -r 1 "You should probably unplug."
	fi

	ICON=""
fi

printf "%s\n" "battery%{F$COLOR}$ICON $CAPACITY%$TIME%{F-}"
