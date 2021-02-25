#!/bin/sh

# Rofi prompt for system functions.

if [ "$WM" = "bspwm" ]; then
	logout="bspc quit"
elif [ "$WM" = "i3" ]; then
	logout="i3-msg exit"
fi

commands="\
 Lock:$HOME/.scripts/wm/lock.sh
 Suspend:systemctl suspend
 Shutdown:systemctl poweroff
 Reboot:systemctl reboot
 Logout:$logout"

choice=$(echo "$commands" | cut -d ':' -f 1 | rofi -dmenu -i -p "System") || exit 0

exec=$(echo "$commands" | grep "^$choice" | cut -d ':' -f 2-)

eval "$exec"
