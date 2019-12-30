## Settings

# Directories
export CAPTURE="$HOME/pictures/screen-captures"
export FPATH="$FPATH:$HOME/.scripts/completion"
export PATH="$PATH:$HOME/.scripts"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
mkdir -p "$XDG_CACHE_HOME/zsh"

# Editor
export EDITOR="vim"

# Files
export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc"
export PLATFORMIO_HOME_DIR="$XDG_DATA_HOME/platformio"
export R_ENVIRON_USER="$XDG_CONFIG_HOME/R/Renviron"

# GTK
export GDK_SCALE=2
export GDK_DPI_SCALE=0.5

# Less
export LESS="-R"
export LESSHISTFILE="-"
export LESS_TERMCAP_mb="$(printf '%b' '\e[01;31m')"     # begin blink
export LESS_TERMCAP_md="$(printf '%b' '\e[01;34m')"     # begin bold
export LESS_TERMCAP_me="$(printf '%b' '\e[0m')"         # reset blink/bold
export LESS_TERMCAP_so="$(printf '%b' '\e[01;107;30m')" # begin reverse video
export LESS_TERMCAP_se="$(printf '%b' '\e[0m')"         # reset reverse video
export LESS_TERMCAP_us="$(printf '%b' '\e[04;95m')"     # begin underline
export LESS_TERMCAP_ue="$(printf '%b' '\e[0m')"         # reset underline

# Panel
export PANEL_HEIGHT=38
export PANEL_NAME="polybar_panel"
export PANEL_PIPE="/tmp/lemonbar_pipe"

# Qt5
export QT_AUTO_SCREEN_SCALE_FACTOR=0
export QT_SCREEN_SCALE_FACTORS=2
export QT_QPA_PLATFORMTHEME="qt5ct"

# Terminal
export TERMINAL="urxvt"

# Vim
export VIMINIT="source $XDG_CONFIG_HOME/vim/vimrc"

# WM
export WM="i3"

# Xorg
export XINITRC="$XDG_CONFIG_HOME/xorg/xinitrc"
export XAUTHORITY="$XDG_DATA_HOME/xorg/Xauthority"

# Colors, window manager colors, workspace names
# Example: *.color0:        #282a2e  ->  COLOR0=#282a2e
# Example: WmColor.bgcolor: #404552  ->  BGCOLOR=#404552
# Example: WmWorkSpace.ws0: "10"     ->  WS0="10"
EXPORTS=$(sed -nE \
	-e 's/^\*.(\w+): *#/\U\1=#/p' \
	-e 's/^WmColor.(\w+): */\U\1=/p' \
	-e 's/^WmWorkSpace.(\w+): */\U\1=/p' \
	"$XDG_CONFIG_HOME/xorg/Xresources")
echo "$EXPORTS" | while read -r line; do
	export $line
done

## Login

[ "$USER" = "rick" ] && [ "$(tty)" = "/dev/tty1" ] && exec xinit -- vt1 > /dev/null 2>&1
