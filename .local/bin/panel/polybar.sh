#!/bin/sh

# Kill existing panels
while [ "$(pgrep -cx polybar)" -gt 1 ]; do
	pkill --exact --signal 9 polybar
done

# Start a panel on each monitor
for m in $(polybar --list-monitors | cut -d ":" -f1); do
	MONITOR="$m" setsid -f polybar --reload polybar
done
