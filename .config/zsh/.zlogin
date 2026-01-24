XORG_USER='rick'

if [ "$WAYLAND" = true ]; then
	[ "$USER" = "$XORG_USER" ] && [ "$(tty)" = "/dev/tty1" ] && exec start-hyprland > /dev/null 2>&1
else
	[ "$USER" = "$XORG_USER" ] && [ "$(tty)" = "/dev/tty1" ] && exec xinit -- vt1 > /dev/null 2>&1
fi
