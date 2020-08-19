XORG_USER='rick'

[ "$USER" = "$XORG_USER" ] && [ "$(tty)" = "/dev/tty1" ] && exec xinit -- vt1 > /dev/null 2>&1
