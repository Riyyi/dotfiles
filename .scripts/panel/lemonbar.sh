#!/bin/sh

PIPE="/tmp/lemonbar_pipe"
PANEL="lemonbar_panel"

title() {
	# Grabs focused window's title
	ID="$(printf "%s" "$1" | awk '{print $5}')"
	if [ "$ID" == "0x0" ]; then
		TITLE=""
	else
		TITLE="$(xprop -id "$ID" WM_NAME \
			| awk '{$1=$2=""; print substr($0, 4, length($0) - 4)}')"
	fi
	printf "%s\n" "title$TITLE"

	# Grabs focused window's title
	# TITLE=$(xdotool getactivewindow getwindowname 2> /dev/null)
	# printf "%s\n" "title$TITLE"
}

clock() {
	# Grabs date in 11:59 A/PM format
	DATETIME=$(date "+%I:%M %p")
	printf "%s\n" "clock $DATETIME"
}

bar() {
	lemonbar \
		-a 20 -g x38 -n "$PANEL" \
		-f "DejaVu Sans-8" \
		-f "FontAwesome5Free Solid-8" \
		-f "FontAwesome5Free Regular-8" \
		-f "FontAwesome5 Brands-8" \
		-B "$BGCOLOR_INACTIVE" -F "$COLOR15"
}

start() {
	# Exit if panel is already running
	if [ "$(pgrep lemonbar | awk 'END {print FNR}')" -gt "2" ]; then
		printf "%s\n" "The panel is already running." >&2
		exit 1
	fi

	# Trap all subshells
	trap 'trap - TERM; kill 0' INT TERM QUIT EXIT

	# Create named pipe
	[ -e "$PIPE" ] && rm "$PIPE"
	mkfifo "$PIPE"

	# Directory of this script
	DIR="$(dirname "$0")"

	# Setup workspaces with xprop events
	xprop -root -spy _NET_CURRENT_DESKTOP | while read -r line; do
		"$DIR"/workspaces.sh
	done > "$PIPE" &

	# Setup window title with xprop events
	xprop -root -spy _NET_ACTIVE_WINDOW | while read -r line; do
		title "$line"
	done > "$PIPE" &

	# Setup interrupt blocks
	"$DIR"/volume.sh
	"$DIR"/brightness.sh

	# Setup block timers
	while :; do "$DIR"/wifi.sh;    sleep 10; done > "$PIPE" &
	while :; do "$DIR"/iface.sh;   sleep 10; done > "$PIPE" &
	while :; do "$DIR"/battery.sh; sleep 30; done > "$PIPE" &
	while :; do clock;             sleep 5; done > "$PIPE" &

	while read -r line ; do
		case $line in
			workspaces*)
				workspaces=${line#workspaces}
				;;
			title*)
				title=${line#title}
				;;
			volume*)
				volume=${line#volume}
				;;
			brightness*)
				brightness=${line#brightness}
				;;
			wifi*)
				wifi=${line#wifi}
				;;
			iface*)
				iface=${line#iface}
				;;
			battery*)
				battery=${line#battery}
				;;
			clock*)
				clock=${line#clock}
				;;
		esac
		printf "%s\n" "%{l}$workspaces%{c}$title%{r}$volume   $brightness   $wifi   $iface   $battery   $clock "
	done < "$PIPE" | bar | sh

	wait
}

getpipe() {
	printf "%s" "$PIPE"
}

if type "$1" 2> /dev/null | grep -q "function"; then
	"$@"
else
	start
fi
