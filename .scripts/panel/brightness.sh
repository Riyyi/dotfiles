#!/bin/sh

BRIGHTNESS_CONTROL="$HOME/.scripts/brightnesscontrol.sh"

# Right click, scroll up, scroll down
R="A3:$BRIGHTNESS_CONTROL s 30:"
U="A4:$BRIGHTNESS_CONTROL u 10:"
D="A5:$BRIGHTNESS_CONTROL d 10:"

PERCENTAGE=$(brightnessctl \
	| awk '/\([0-9]+%\)/ { print substr($4, 2, length($4) - 3) }')
if [ "$PERCENTAGE" -ge "75" ]; then
	ICON="" # f111
elif [ "$PERCENTAGE" -ge "25" ]; then
	ICON="" # f042
else
	ICON="" # f1ce
	# ICON="" # f111
fi

INPUT="%{$R}%{$U}%{$D}"
END="%{A}%{A}%{A}"
PIPE="$($(dirname $0)/lemonbar.sh getpipe)"

printf "%s\n" "brightness$INPUT$ICON $PERCENTAGE%$END" > "$PIPE" &
