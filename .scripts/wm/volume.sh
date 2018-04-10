#!/bin/sh

# default (ALSA), pulse, jack
MIXER="pulse"
INSTANCE=${BLOCK_INSTANCE:-"Master"}
STEP="5%"

case $BLOCK_BUTTON in
	1) pavucontrol ;; # left click, start sound settings
	2) amixer -q -D $MIXER sset $INSTANCE 0 unmute ;; # scroll click, set 0
	3) amixer -q -D $MIXER sset $INSTANCE toggle ;; # right click, mute/unmute
	4) amixer -q -D $MIXER sset $INSTANCE ${STEP}+ unmute ;; # scroll up, increase
	5) amixer -q -D $MIXER sset $INSTANCE ${STEP}- unmute ;; # scroll down, decrease
esac

INFO="amixer -D $MIXER get $INSTANCE"

volume() {
	VOLUME="$(${INFO} | \
		awk '/\[[0-9]+%\]/ { print substr($5, 2, length($5) - 3); exit }')"
}

symbol() {

	if [ "$VOLUME" -ge "50" ]; then
		SYMBOL=""
	elif [ "$VOLUME" -ge "25" ]; then
		SYMBOL=""
	else
		SYMBOL=""
	fi
}

setOutput() {
	# If sound is not muted
	if [ -n "$(${INFO} | sed -nr 's/(\[on\])/\1/p')" ]; then
		COLOR="#FFF"
		volume
		symbol
		VOLUME="$VOLUME%"
	else
		COLOR="#676E7D"
		VOLUME="MUTE"
		SYMBOL=""
	fi
}

setOutput

echo "<span color='$COLOR'>$SYMBOL</span> $VOLUME"

