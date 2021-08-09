## Terminal

# Disable Ctrl+S and Ctrl+Q
stty -ixon

# Set tab width
tabs -4

# vi mode
bindkey -v

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
compinit -d "$XDG_CACHE_HOME/zsh/zcompdump"
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path "$XDG_CACHE_HOME/zsh/zcompcache"
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' menu select

# Bind keys
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end

bindkey '\eOc' forward-word                               # ctrl-right
bindkey '\eOd' backward-word                              # ctrl-left
bindkey '\e[3~' delete-char                               # del
bindkey '\e[7~' beginning-of-line                         # home
bindkey '\e[8~' end-of-line                               # end
bindkey '\e[A' history-beginning-search-backward-end      # up
bindkey '\e[B' history-beginning-search-forward-end       # down
bindkey '\e[Z' reverse-menu-complete                      # shift-tab
bindkey '\eh' kill-whole-line                             # meta-h
bindkey '\ej' history-beginning-search-forward-end        # meta-j
bindkey '\ek' history-beginning-search-backward-end       # meta-k
bindkey '\el' accept-line                                 # meta-l
bindkey '^R' history-incremental-pattern-search-backward  # ctrl-r

# History
setopt APPEND_HISTORY
setopt EXTENDED_HISTORY
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_DUPS
setopt HIST_REDUCE_BLANKS
setopt HIST_SAVE_NO_DUPS
HISTFILE="$XDG_CACHE_HOME/zsh/zsh_history"
HISTSIZE=10000
SAVEHIST=10000

## Aliases

# General
alias ..="cd .."
alias ...="cd ../.."
alias ..2="cd ../.."
alias ..3="cd ../../.."
alias ..4="cd ../../../.."
alias cp='cp -i'
alias df="df -h"
alias du="du -h"
alias e="aliases emacs"
alias emacs="aliases emacs"
alias fuck='sudo $(fc -ln -1)'
alias grep="grep --color=always"
alias ip="ip --color"
alias ipb="ip --color --brief a"
alias l.="\ls -lAGh --color --group-directories-first | awk -v r='^(.*m)?\\\.' '{ if (\$8 ~ r) print; }'"
alias la="\ls -lAGh --color --group-directories-first"
alias less="less -x 4"
alias ls="ls --color --group-directories-first"
alias md="mkdir -p"
alias mv='mv -i'
alias pkill="pkill -9"
alias q="exit"
alias rm="rm -i"
alias se="sudoedit"
alias semacs="sudoedit"
alias ss="sudo systemctl"
alias v="vim --servername VIM"
alias vim="vim --servername VIM"

# Config
alias c="config"
alias zshrc="$EDITOR $ZDOTDIR/.zshrc && source $ZDOTDIR/.zshrc"

alias lxrdb="xrdb $XDG_CONFIG_HOME/xorg/Xresources"
alias lzshrc="source $ZDOTDIR/.zshrc"

# Pacman
alias cache="sudo paccache -r -k 2"
alias clean='sudo pacman -Rns $(pacman -Qtdq)'
alias depend="aliases depend"
alias download="sudo pacman -Syuw --noconfirm"
alias install="sudo pacman -S --needed"
alias remove="sudo pacman -Rns"
alias search="aliases pacman_search"
alias update="sudo pacman -Syyu"
alias update-mirrorlist="aliases update_mirrorlist"

# Programming
alias jdoc="aliases java_doc"
alias jr="aliases java_run"
alias raspbian="aliases raspbian"
alias ser="pio device monitor -b 9600"
alias upl="pio run -t upload"
alias qmake="qmake -makefile ../ && make"

# Git
alias g="git"
alias ga="git add"
alias gap="git add -p"
alias gc="git commit"
alias gch="git checkout"
alias gd="git diff"
alias gdc="git diff --cached"
alias gds="git diff --staged"
alias gl="git log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold cyan)%ai%C(reset) %C(bold green)(%ar)%C(reset)%C(bold yellow)%d    %C(reset)%n''          %C(white)%s%C(reset) %C(dim white)- %an%C(reset)' --all"
alias gle="git log --graph --stat --format=format:'%C(bold blue)commit %H%C(reset)%C(bold yellow)%d %C(reset)%nAuthor: %C(dim white)%an <%ae>%C(reset)%nDate:   %C(bold cyan)%ai%C(reset) %C(bold green)(%ar)%C(reset)%n%n%w(64,4,4)%B'"
alias gp="git pull"
alias gps="git push"
alias gpsa="git remote | xargs -I remotes git push remotes master"
alias gr="git reset"
alias gs="git status"
alias gsh="git show --format=format:'%C(bold blue)commit %H%C(reset) %C(bold yellow)%d %C(reset)%nAuthor: %C(dim white)%an <%ae>%C(reset)%nDate:   %C(bold cyan)%ai%C(reset) %C(bold green)(%ar)%C(reset)%n%n%w(64,4,4)%B'"
alias gt='git ls-tree -r --name-only master .'

# Scripts
alias nc="netictl"
alias vc="volctl"
alias vp="vimplugin"

# Other
alias crypto="curl 'https://rate.sx/?qF'"
alias IP="curl https://ifconfig.me"
alias len="xclip -o | wc -m"
alias length="rofi -dmenu -i -p 'String length' -lines 0 | tr -d '\n' | wc -m"
alias p="aliases pastebin"
alias screencast="aliases screencast"
alias stream="aliases stream"
alias u="setsid -f urxvt -cd $PWD"
alias weather="curl -s 'https://wttr.in/dordrecht?q&n&p' | head -n -3"
alias webmconvert="aliases webmconvert"
alias workbench="GDK_SCALE=1 GDK_DPI_SCALE=1 setsid -f -- mysql-workbench > /dev/null 2>&1"

mkcd() { mkdir -p -- "$1" && cd -P -- "$1" || exit; }

highlighting="/usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
[ -f "$highlighting" ] && source "$highlighting"; unset highlighting

[ -f "$ZDOTDIR/.zshrc_extended" ] && source "$ZDOTDIR/.zshrc_extended"
