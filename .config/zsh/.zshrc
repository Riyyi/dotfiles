## Settings

# Directories
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"

# VIM
export EDITOR="vim"

# X11
export XINITRC="$XDG_CONFIG_HOME/xorg/xinitrc"
export XAUTHORITY="$XDG_DATA_HOME/xorg/Xauthority"

## ZSH

autoload -Uz promptinit colors vcs_info compinit

# Prompt
promptinit
colors
setopt INTERACTIVE_COMMENTS
setopt PROMPT_SUBST

precmd() {
	vcs_info
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
bindkey '\eOd' backward-word
bindkey '\eOc' forward-word
bindkey '\e[7~' beginning-of-line
bindkey '\e[3~' delete-char
bindkey '\e[8~' end-of-line

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
alias la='ls -laGh --color'
alias ls='ls --color'
alias rm='rm -i'
alias zrc='nano $ZDOTDIR/.zshrc'
alias lzrc='source $ZDOTDIR/.zshrc'

alias install='sudo pacman -S'
alias remove='sudo pacman -Rns'
alias update='sudo pacman -Syy; sudo pacman -Syu'
alias clean='sudo pacman -Rns $(pacman -Qtdq)'
alias depend='pacman -Qtd'

alias poweroff='systemctl poweroff'
alias reboot='systemctl reboot'

alias upl='pio run --target=upload'
alias ser='pio serialports monitor -b 9600'

# Git
alias g="git"
alias ga="g add"
alias gs="g status"
alias gc="g commit"
alias gp="g push"
alias gd="g diff"
alias gdc="gd --cached"
alias gl="g log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold cyan)%aD%C(reset) %C(bold green)(%ar)%C(reset)%C(bold yellow)%d    %C(reset)%n''          %C(white)%s%C(reset) %C(dim white)- %an%C(reset)' --all"

# Laptop
alias offtouchpad='sudo rmmod i2c_hid'
alias ontouchpad='sudo modprobe i2c_hid'
alias nw="$HOME/.scripts/network.sh"

# Other
source $ZDOTDIR/.zshrc_extended

# Fun
spurdo() {
    sed "s/kek/geg/gI;s/epic/ebin/gI;s/america/clapistan/gI;s/right/rite/gI;s/your/ur/gI;s/\./ :DD/gI;s/'//gI;s/,/XDD/gI;s/wh/w/gI;s/th/d/gI;s/af/ab/gI;s/ap/ab/gI;s/ca/ga/gI;s/ck/gg/gI;s/co/go/gI;s/ev/eb/gI;s/ex/egz/gI;s/et/ed/gI;s/iv/ib/gI;s/it/id/gI;s/ke/ge/gI;s/nt/nd/gI;s/op/ob/gI;s/ot/od/gI;s/po/bo/gI;s/pe/be/gI;s/pi/bi/gI;s/up/ub/gI;s/va/ba/gI;s/ck/gg/gI;s/cr/gr/gI;s/kn/gn/gI;s/lt/ld/gI;s/mm/m/gI;s/nt/dn/gI;s/pr/br/gI;s/ts/dz/gI;s/tr/dr/gI;s/bs/bz/gI;s/ds/dz/gI;s/es/es/gI;s/fs/fz/gI;s/gs/gz/gI;s/ is/iz/gI;s/as/az/gI;s/ls/lz/gI;s/ms/mz/gI;s/ns/nz/gI;s/rs/rz/gI;s/ss/sz/gI;s/ts/tz/gI;s/us/uz/gI;s/ws/wz/gI;s/ys/yz/gI;s/alk/olk/gI;s/ing/ign/gI;s/ic/ig/gI;s/ng/nk/gI" \
    <<< "${@:-$(cat /dev/stdin)}"
}

## Login

[[ $USER == "rick" ]] && [ "$(tty)" = "/dev/tty1" ] && exec xinit -- vt1 &> /dev/null
