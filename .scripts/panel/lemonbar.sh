#!/bin/sh

PIPE="/tmp/lemonbar_pipe"
PANEL="lemonbar_panel"

title() {
	# Grab focused window's ID
	if [ -n "$1" ]; then
		ID="$(printf "%s" "$1" | cut -f 2)"
	else
		ID="$(xprop -root '\t$0' _NET_ACTIVE_WINDOW | cut -f 2)"
	fi

	# Grabs focused window's title
	if [ "$ID" = "0x0" ]; then
		TITLE=""
	else
		TITLE="$(xprop -id "$ID" ' $0' WM_NAME | awk '{
			$1=""; $0=substr($0, 3, length($0) - 3);
			if (length($0) <= 75)
				print $0
			else
				print substr($0, 1, 72)"..."
		}')"
	fi
	printf "%s\n" "title$TITLE"
}

clock() {
	# Grabs date in 11:59 A/PM format
	DATETIME=$(date "+%I:%M %p")
	printf "%s\n" "clock $DATETIME"
}

bar() {
	lemonbar \
		-a 20 -g x38 -n "$PANEL" \
		-f "DejaVu Sans-8" -o 0 \
		-f "FontAwesome5Free Solid-8" -o -3 \
		-f "FontAwesome5Free Regular-8" -o -3 \
		-f "FontAwesome5 Brands-8" -o -3 \
		-B "$BGCOLOR_INACTIVE" -F "$COLOR15"
}

start() {
	# Kill existing panel
	while [ "$(pgrep -cx lemonbar.sh)" -gt 1 ]; do
		pkill -ox -9 lemonbar.sh;
	done

	# Trap all subshells
	trap 'trap - TERM; kill 0' INT TERM QUIT EXIT

	# Create named pipe
	[ -e "$PIPE" ] && rm "$PIPE"
	mkfifo "$PIPE"

	# Directory of this script
	DIR="$(dirname "$0")"

	# Setup workspaces block with xprop events
	xprop -root -spy _NET_CURRENT_DESKTOP _NET_NUMBER_OF_DESKTOPS | while read -r line; do
		"$DIR"/workspaces.sh
	done > "$PIPE" &

	# Setup window title block with xprop events
	xprop -root -spy '\t$0\n' _NET_ACTIVE_WINDOW | while read -r line; do
		title "$line"
	done > "$PIPE" &

	# Setup interrupt blocks
	"$DIR"/volume.sh
	"$DIR"/brightness.sh

	# Setup block timers
	while :; do title;             sleep 1; done > "$PIPE" &
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
