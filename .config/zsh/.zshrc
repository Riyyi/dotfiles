## Settings

# Directories
export PATH="$PATH:$HOME/.scripts"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"

# Files
export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc"
export PLATFORMIO_HOME_DIR="$XDG_DATA_HOME/platformio"

# Qt5
export QT_QPA_PLATFORMTHEME="qt5ct"

# VIM
export EDITOR="vim"
export VIMINIT="source $XDG_CONFIG_HOME/vim/vimrc"

# X11
export XINITRC="$XDG_CONFIG_HOME/xorg/xinitrc"
export XAUTHORITY="$XDG_DATA_HOME/xorg/Xauthority"

# General
export LESSHISTFILE=-

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

# ZSH parameters
USR_HOST="%F{cyan}%n%f@%F{cyan}%m%f"
DIRECTORY="%F{green}%~%f"
ARROW="%(?..%F{red})➤%f"
PROMPT='╭─${USR_HOST} ${DIRECTORY} ${vcs_info_msg_0_}
╰─${ARROW} '
RPROMPT='%t'
TIMEFMT=$'\nreal\t%*Es\nuser\t%*Us\nsys\t%*Ss'

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

# General
alias q='exit'
alias la='ls -laGh --color --group-directories-first'
alias ls='ls --color --group-directories-first'
alias rm='rm -i'
alias vim='vim --servername VIM'
alias svim='sudoedit'
alias fuck='sudo $(fc -ln -1)'

# Config
alias vrc="$EDITOR $XDG_CONFIG_HOME/vim/vimrc"
alias zrc="$EDITOR $ZDOTDIR/.zshrc && source $ZDOTDIR/.zshrc"
alias lzrc="source $ZDOTDIR/.zshrc"

# Pacman
alias install='sudo pacman -S --needed'
alias remove='sudo pacman -Rns'
alias update='sudo pacman -Syyu'
alias clean='sudo pacman -Rns $(pacman -Qtdq)'
alias cache='sudo paccache -r'
alias depend="$HOME/.scripts/alias.sh depend"

# Programming
alias jdoc="$HOME/.scripts/alias.sh java-doc"
alias jr="$HOME/.scripts/alias.sh java-run"
alias raspbian="$HOME/.scripts/alias.sh raspbian"
alias ser='pio serialports monitor -b 9600'
alias upl='pio run -t upload'
alias qmake='qmake -makefile ../ && make'

# Git
alias g="git"
alias ga="git add"
alias gs="git status"
alias gc="git commit"
alias gp="git pull"
alias gps="git push"
alias gd="git diff"
alias gdc="git diff --cached"
alias gl="git log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold cyan)%aD%C(reset) %C(bold green)(%ar)%C(reset)%C(bold yellow)%d    %C(reset)%n''          %C(white)%s%C(reset) %C(dim white)- %an%C(reset)' --all"

# Scripts
alias mc="$HOME/.scripts/mediacontrol.sh"
alias nw="$HOME/.scripts/network.sh"
alias vp="$HOME/.scripts/vimplugin.sh"

# Laptop
alias offtouchpad='sudo rmmod i2c_hid'
alias ontouchpad="sudo modprobe i2c_hid && $HOME/.scripts/touchscreen.sh 0"

# Other
alias man="$HOME/.scripts/alias.sh man"
alias mysql-workbench="GDK_SCALE=1 GDK_DPI_SCALE=1 mysql-workbench 1>/dev/null 2>&1 &; disown"
alias weather="curl -s 'http://wttr.in/dordrecht?q&n&p' | head -n -3"

source $ZDOTDIR/.zshrc_extended

## Login

[[ $USER == "rick" ]] && [ "$(tty)" = "/dev/tty1" ] && exec xinit -- vt1 &> /dev/null
