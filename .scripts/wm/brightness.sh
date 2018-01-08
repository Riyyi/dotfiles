#!/bin/sh

case $BLOCK_BUTTON in
	3) xbacklight -set 30 ;; # right click
	4) xbacklight -inc 10 ;; # scroll up
	5) xbacklight -dec 10 ;; # scroll down
esac

PERCENTAGE=$(printf  "%.0f" "$(xbacklight)")

if [ "$PERCENTAGE" -ge "75" ]; then
	ICON=""
elif [ "$PERCENTAGE" -ge "25" ]; then
	ICON=""
else 
	ICON=""
fi

echo $ICON $PERCENTAGE%

