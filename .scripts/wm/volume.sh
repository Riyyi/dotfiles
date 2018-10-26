#!/bin/sh

MEDIACONTROL="$HOME/.scripts/mediacontrol.sh"

case $BLOCK_BUTTON in
	1) pavucontrol ;;           # Left click
	2) $MEDIACONTROL set 0 ;;   # Scroll click
	3) $MEDIACONTROL toggle ;;  # Right click
	4) $MEDIACONTROL up 5 ;;    # Scroll up
	5) $MEDIACONTROL down 5 ;;  # Scroll down
esac

if [ "$($MEDIACONTROL getmute)" == "0" ]; then
	COLOR="$COLOR7"
	VOLUME="MUTE"
	SYMBOL=""
else
	COLOR="$COLOR15"
	VOLUME="$($MEDIACONTROL getvolume)%"

	if [ "${VOLUME%?}" -ge "50" ]; then
		SYMBOL=""
	elif [ "${VOLUME%?}" -ge "25" ]; then
		SYMBOL=""
	else
		SYMBOL=""
	fi
fi

echo "<span color='$COLOR'>$SYMBOL $VOLUME</span>"
