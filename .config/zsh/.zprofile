#--- General ---#

# Directories
export CAPTURE="$HOME/pictures/screen-captures"
export FPATH="$FPATH:$HOME/.local/completion"
export PATH="$PATH:$HOME/.local/bin"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
mkdir -p "$XDG_CACHE_HOME/zsh"

# Editor
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient"
export VISUAL="emacsclient"

# Files
export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc"
export PLATFORMIO_HOME_DIR="$XDG_DATA_HOME/platformio"
export R_ENVIRON_USER="$XDG_CONFIG_HOME/R/Renviron"

# HiDPI screen
# >>> hostname=arch-laptop
# export HIDPI=true
# <<<

#--- Program Specific ---#

# Android
export ANDROID_HOME="$XDG_DATA_HOME/android"

# GPG
export GNUPGHOME="$XDG_CONFIG_HOME/gnupg"

# GTK
if [ "$HIDPI" = true ]; then
	export GDK_SCALE=2
	export GDK_DPI_SCALE=0.5
fi

# Gradle
export GRADLE_USER_HOME="$XDG_DATA_HOME/gradle"

# Java
export _JAVA_AWT_WM_NONREPARENTING=1

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

# ls (LS_COLORS)
eval "$(dircolors -b)"

# Make
export MAKEFLAGS="-j $(getconf _NPROCESSORS_ONLN)"

# npm
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/npmrc"

# OpenSSL
export RANDFILE="$XDG_CACHE_HOME/rnd"

# Panel
[ "$HIDPI" = true ] && export PANEL_HEIGHT=38 || export PANEL_HEIGHT=22
export PANEL_NAME="polybar_panel"

# Python
export PYTHONSTARTUP="$XDG_CONFIG_HOME/python/pythonrc"

# Qt5
if [ "$HIDPI" = true ]; then
	export QT_AUTO_SCREEN_SCALE_FACTOR=0
	export QT_SCREEN_SCALE_FACTORS=2
fi
export QT_QPA_PLATFORMTHEME="qt5gtk2"

# Rust
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"

# Sudo
export SUDO_ASKPASS="$HOME/.local/bin/rofipass"

# Terminal
export TERMINAL="urxvt"

# Vim
export VIMINIT="source $XDG_CONFIG_HOME/nvim/init.lua"

# Web browser
export BROWSER="firefox"

# Wget
export WGETRC="$XDG_CONFIG_HOME/wget/wgetrc"

# Wine
export WINEPREFIX="$XDG_DATA_HOME/wine"

# WM
export WM="bspwm"

# Xorg
export XINITRC="$XDG_CONFIG_HOME/xorg/xinitrc"
export XAUTHORITY="$XDG_DATA_HOME/xorg/Xauthority"
touch "$XDG_DATA_HOME/xorg/Xauthority"

# Colors, window manager colors, workspace names
# Example: *.color0:        #282a2e  ->  COLOR0=#282a2e
# Example: WmColor.bgcolor: #404552  ->  BGCOLOR=#404552
# Example: WmWorkSpace.ws0: "10"     ->  WS0="10"
exports=$(sed -nE "
	s/^\*.(\w+): *#/\U\1=#/p;
	s/^WmColor.(\w+): */\U\1=/p;
	s/^WmWorkSpace.(\w+): */\U\1=/p;
" "$XDG_CONFIG_HOME/xorg/Xresources")
echo "$exports" | while read -r export; do
	export $export
done
