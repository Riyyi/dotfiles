#!/bin/sh

MAIN_DISPLAY="eDP-1"
MAIN_MODE="--mode 3000x2000_48.00 --primary"

popup() {
	echo "$1" | rofi -dmenu -i -p "$2" \
		-color-window "#524040, #dc7a43, #fff" \
		-color-normal "#524040, #fff, #524040, #fff, #707880" \
		-no-fixed-num-lines
}

multiDisplay() {
	case "$(echo "$CONNECTED" | wc -l)" in
		1) setSingle ;;
		2) setDual ;;
		*) setMulti ;;
	esac
}

setSingle() {
	[ "$CHOOSE" = "$MAIN_DISPLAY" ] && MODE="$MAIN_MODE" || MODE="--auto"

	# Disconnect all other displays
	eval xrandr --output "$CHOOSE" "$MODE" \
		"$(echo "$CONNECTED" | grep -vx "$CHOOSE" | awk '{print "--output", $1, "--off"}' | tr '\n' ' ')"
}

setDual() {
	PRIMARY=$(popup "$CONNECTED" "Select primary display")
	[ "$PRIMARY" = "$MAIN_DISPLAY" ] && MODE="$MAIN_MODE" || MODE="--auto"

	SECONDARY=$(echo "$CONNECTED" | grep -vx "$PRIMARY")
	SIDE=$(popup "$(printf "right\nleft")" "On which side of $PRIMARY should $SECONDARY be?")
	eval xrandr --output "$PRIMARY" "$MODE" --output "$SECONDARY" --"$SIDE"-of "$PRIMARY" --auto
}

setMulti() {
	# @Todo
	echo "Todo"
}

# Get all connected displays
CONNECTED=$(xrandr -q | grep " connected" | awk '{print $1}')

# Add Multi-monitor and Manual selection to the selectable options
SELECTION="$(printf "%s\nMulti-monitor\nManual selection" "$CONNECTED")"

# Get user selection
CHOOSE="$(popup "$SELECTION" "Select display arangement")" &&
case "$CHOOSE" in
	"Manual selection") arandr 2> /dev/null ; exit ;;
	"Multi-monitor") multiDisplay ;;
	*) setSingle ;;
esac

# Reload background
$HOME/.scripts/wm/wallpaper.sh
