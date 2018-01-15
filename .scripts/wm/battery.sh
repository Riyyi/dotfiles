#!/bin/sh

BAT_NUMBER=${BLOCK_INSTANCE:-"0"}
BAT_INFO="$(acpi -b | grep Battery\ $BAT_NUMBER:)"

CAPACITY=$(echo $BAT_INFO | awk '{ print int($4) }')
TIME="$(echo $BAT_INFO | awk '{ print substr($5, 0, length($5) - 3) }')"

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
	fi
else	
	ICON=""
fi

echo "$ICON $CAPACITY% ($TIME)"
echo "$ICON $CAPACITY%"
echo "#FFFFFF"
