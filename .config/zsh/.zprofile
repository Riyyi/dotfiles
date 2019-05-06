## Settings

# Directories
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

# Qt5
export QT_AUTO_SCREEN_SCALE_FACTOR=0
export QT_SCREEN_SCALE_FACTORS=2
export QT_QPA_PLATFORMTHEME="qt5ct"

# Vim
export VIMINIT="source $XDG_CONFIG_HOME/vim/vimrc"

# Xorg
export XINITRC="$XDG_CONFIG_HOME/xorg/xinitrc"
export XAUTHORITY="$XDG_DATA_HOME/xorg/Xauthority"

# Colors
# Example:  *.color0: #282a2e         ->  export COLOR0=#282a2e
$(awk '/^*.color/ {print "export " toupper(substr($1, 3, length($1) - 3)) "=" $2}' \
	"$XDG_CONFIG_HOME/xorg/Xresources")
# Example:  Wmcolor.bgcolor: #404552  ->  export BGCOLOR=#404552
$(awk '/Wmcolor./ {print "export " toupper(substr($1, 9, length($1) - 9)) "=" $2}' \
	"$XDG_CONFIG_HOME/xorg/Xresources")

## Login

[ "$USER" = "rick" ] && [ "$(tty)" = "/dev/tty1" ] && exec xinit -- vt1 > /dev/null 2>&1
