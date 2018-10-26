#!/bin/sh

case $BLOCK_BUTTON in
	3) brightnessctl -q s 30% ;; # right click
	4) brightnessctl -q s +10% ;; # scroll up
	5) brightnessctl -q s 10%- ;; # scroll down
esac

PERCENTAGE=$(brightnessctl | awk '/\([0-9]+%\)/ { print substr($4, 2, length($4) - 3) }')

if [ "$PERCENTAGE" -ge "75" ]; then
	ICON=""
elif [ "$PERCENTAGE" -ge "25" ]; then
	ICON=""
else
	ICON=""
fi

echo "$ICON $PERCENTAGE%"
echo "$ICON $PERCENTAGE%"
echo "$COLOR15"
