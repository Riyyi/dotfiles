#!/bin/sh

MEDIACONTROL="$HOME/.scripts/mediacontrol.sh"

# Left click, scroll click, right click, scroll up and scroll down
L="A1:pavucontrol:"
S="A2:$MEDIACONTROL set 0; $0:"
R="A3:$MEDIACONTROL toggle; $0:"
U="A4:$MEDIACONTROL up 5; $0:"
D="A5:$MEDIACONTROL down 5; $0:"

VOLUME="$($MEDIACONTROL getvolume)"

COLOR="-"
if [ "$($MEDIACONTROL getmute)" = "1" ]; then
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
PIPE="$($(dirname $0)/lemonbar.sh getpipe)"

printf "%s\n" "volume%{F$COLOR}$INPUT$SYMBOL $VOLUME%$END%{F-}" > "$PIPE" &
