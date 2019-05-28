#!/bin/sh

MEDIA_CONTROL="$HOME/.scripts/mediacontrol.sh"

# Left click, scroll click, right click, scroll up and scroll down
L="A1:pavucontrol:"
S="A2:$MEDIA_CONTROL set 0:"
R="A3:$MEDIA_CONTROL toggle:"
U="A4:$MEDIA_CONTROL up 5:"
D="A5:$MEDIA_CONTROL down 5:"

VOLUME="$($MEDIA_CONTROL getvolume)"

COLOR="-"
if [ "$($MEDIA_CONTROL getmute)" = "true" ]; then
	COLOR="$COLOR7"

	SYMBOL=""
else
	COLOR="$COLOR15"

	if [ "$VOLUME" -ge "50" ]; then
		SYMBOL=""
	elif [ "$VOLUME" -ge "25" ]; then
		SYMBOL=""
	else
		SYMBOL=""
	fi
fi

INPUT="%{$L}%{$S}%{$R}%{$U}%{$D}"
END="%{A}%{A}%{A}%{A}%{A}"
PIPE="$("$(dirname "$0")"/lemonbar.sh get_pipe)"

printf "%s\n" "volume%{F$COLOR}$INPUT$SYMBOL $VOLUME%$END%{F-}" > "$PIPE" &
