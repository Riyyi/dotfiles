## Settings

# Directories
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"

# Files
export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc"
export PLATFORMIO_HOME_DIR="$XDG_DATA_HOME/platformio"

# VIM
export EDITOR="vim"
export VIMINIT="source $XDG_CONFIG_HOME/vim/vimrc"

# X11
export XINITRC="$XDG_CONFIG_HOME/xorg/xinitrc"
export XAUTHORITY="$XDG_DATA_HOME/xorg/Xauthority"

# Qt5
export QT_QPA_PLATFORMTHEME="qt5ct"

## ZSH

autoload -Uz promptinit colors vcs_info compinit history-search-end

# Prompt
promptinit
colors
setopt INTERACTIVE_COMMENTS
setopt PROMPT_SUBST

precmd() {
	vcs_info

	print -Pn "\e]0;%n@%m %~\a"
}

USR_HOST="%F{cyan}%n%f@%F{cyan}%m%f"
DIRECTORY="%F{green}%~%f"
ARROW="%(?..%F{red})➤%f"
PROMPT='╭─${USR_HOST} ${DIRECTORY} ${vcs_info_msg_0_}
╰─${ARROW} '
RPROMPT='%t'

# Git
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' stagedstr "%F{green}A%f"
zstyle ':vcs_info:*' unstagedstr "%F{red}M%f"
zstyle ':vcs_info:*' formats '%F{cyan}(%F{red}%b%F{cyan})%f %c%u'

# Autocompletion
compinit -d $XDG_CACHE_HOME/zcompdump
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path $XDG_CACHE_HOME/zcache
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' menu select

# Bind keys
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end

bindkey '\eOd' backward-word
bindkey '\eOc' forward-word
bindkey '\e[7~' beginning-of-line
bindkey '\e[3~' delete-char
bindkey '\e[8~' end-of-line
bindkey '\e[A' history-beginning-search-backward-end
bindkey '\e[B' history-beginning-search-forward-end
bindkey '^R' history-incremental-pattern-search-backward

# History
setopt APPEND_HISTORY
setopt EXTENDED_HISTORY
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_DUPS
setopt HIST_REDUCE_BLANKS
setopt HIST_SAVE_NO_DUPS
HISTFILE=$ZDOTDIR/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

## Aliases

alias q='exit'
alias la='ls -laGh --color --group-directories-first'
alias ls='ls --color --group-directories-first'
alias rm='rm -i'
alias vim='vim --servername VIM'
alias svim='sudoedit'
alias zrc='$EDITOR $ZDOTDIR/.zshrc && source $ZDOTDIR/.zshrc'
alias lzrc='source $ZDOTDIR/.zshrc'
alias fuck='sudo $(fc -ln -1)'

alias install='sudo pacman -S'
alias remove='sudo pacman -Rns'
alias update='sudo pacman -Syy; sudo pacman -Syu'
alias clean='sudo pacman -Rns $(pacman -Qtdq)'
alias depend='pacman -Qtd'
alias cache='paccache -r'

alias poweroff='systemctl poweroff'
alias reboot='systemctl reboot'

alias upl='pio run --target=upload'
alias ser='pio serialports monitor -b 9600'

jr() { mkdir -p "./out"; javac -d "./out" "$1.java"; java -cp "./out" "$1" }
raspberry() {
	sudo systemctl start avahi-daemon.service
	if ! ip a show usb0 | grep -q 'inet6'; then
		sudo dhcpcd usb0
	fi
	ssh -6 pi@$(avahi-resolve-host-name raspberrypi.local | awk '{ print $2 }')%usb0
	sudo systemctl stop avahi-daemon.service
}
alias qmake='qmake -makefile ../ && make'

# Programs
alias mysql-workbench="GDK_SCALE=1 GDK_DPI_SCALE=1 mysql-workbench 1>/dev/null 2>&1 &; disown"

# Git
alias g="git"
alias ga="g add"
alias gs="g status"
alias gc="g commit"
alias gp="g pull"
alias gps="g push"
alias gd="g diff"
alias gdc="gd --cached"
alias gl="g log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold cyan)%aD%C(reset) %C(bold green)(%ar)%C(reset)%C(bold yellow)%d    %C(reset)%n''          %C(white)%s%C(reset) %C(dim white)- %an%C(reset)' --all"

# Laptop
alias offtouchpad='sudo rmmod i2c_hid'
alias ontouchpad='sudo modprobe i2c_hid && $HOME/.scripts/touchscreen.sh 0'
alias nw="$HOME/.scripts/network.sh"

# Scripts
alias ltx="$HOME/.scripts/latex.sh"
alias vimplugin="$HOME/.scripts/vimplugin.sh"

# Other
alias weather="curl -s 'http://wttr.in/Dordrecht?q&n&p' | head -n -3"

source $ZDOTDIR/.zshrc_extended

## Login

[[ $USER == "rick" ]] && [ "$(tty)" = "/dev/tty1" ] && exec xinit -- vt1 &> /dev/null
