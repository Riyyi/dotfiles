## Terminal

# Disable Ctrl+S and Ctrl+Q
stty -ixon

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
HISTFILE="$XDG_CACHE_HOME/zsh/zsh_history"
HISTSIZE=10000
SAVEHIST=10000

## Aliases

# General
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias df="df -h"
alias e="$HOME/.scripts/alias.sh emacs"
alias emacs="$HOME/.scripts/alias.sh emacs"
alias fuck='sudo $(fc -ln -1)'
alias grep="grep --color=always"
alias la="ls -lAGh --color --group-directories-first"
alias less="less -x 4"
alias ls="ls --color --group-directories-first"
alias md="mkdir -p"
alias mkcd="$HOME/.scripts/alias.sh mkcd"
alias pkill="pkill -9"
alias q="exit"
alias rm="rm -i"
alias se="sudoedit"
alias semacs="sudoedit"
alias ss="sudo systemctl"
alias v="vim --servername VIM"
alias vim="vim --servername VIM"

# Config
alias c="$HOME/.scripts/config.sh"
alias zrc="$EDITOR $ZDOTDIR/.zshrc && source $ZDOTDIR/.zshrc"

alias lxrdb="xrdb $XDG_CONFIG_HOME/xorg/Xresources"
alias lzshrc="source $ZDOTDIR/.zshrc"

# Pacman
alias install="sudo pacman -S --needed"
alias remove="sudo pacman -Rns"
alias update="sudo pacman -Syyu"
alias clean='sudo pacman -Rns $(pacman -Qtdq)'
alias cache="sudo paccache -r -k 2"
alias depend="$HOME/.scripts/alias.sh depend"
alias search="$HOME/.scripts/alias.sh pacman_search"

# Programming
alias jdoc="$HOME/.scripts/alias.sh java_doc"
alias jr="$HOME/.scripts/alias.sh java_run"
alias raspbian="$HOME/.scripts/alias.sh raspbian"
alias ser="pio device monitor -b 9600"
alias upl="pio run -t upload"
alias qmake="qmake -makefile ../ && make"

# Git
alias g="git"
alias ga="git add"
alias gap="git add -p"
alias gs="git status"
alias gc="git commit"
alias gp="git pull"
alias gps="git push"
alias gpsa="git remote | xargs -I remotes git push remotes master"
alias gd="git diff"
alias gdc="git diff --cached"
alias gl="git log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold cyan)%aD%C(reset) %C(bold green)(%ar)%C(reset)%C(bold yellow)%d    %C(reset)%n''          %C(white)%s%C(reset) %C(dim white)- %an%C(reset)' --all"

# Scripts
alias nw="$HOME/.scripts/network.sh"
alias vc="$HOME/.scripts/volctl.sh"
alias vp="$HOME/.scripts/vimplugin.sh"
alias mpvshuffle="$HOME/.scripts/mpv.sh shuffle"



# Other
alias len="xclip -o | wc -m"
alias length="rofi -dmenu -i -p 'String length' -lines 0 | tr -d '\n' | wc -m"
alias p="$HOME/.scripts/alias.sh pastebin"
alias screencast="$HOME/.scripts/alias.sh screencast"
alias stream="$HOME/.scripts/alias.sh stream"
alias weather="curl -s 'https://wttr.in/dordrecht?q&n&p' | head -n -3"
alias webmconvert="$HOME/.scripts/alias.sh webmconvert"
alias workbench="GDK_SCALE=1 GDK_DPI_SCALE=1 mysql-workbench > /dev/null 2>&1 & disown"
alias ytdl="youtube-dl -f bestvideo+bestaudio"
alias ytaudio="youtube-dl -f bestaudio/best -x --audio-format mp3"

zsh="/usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
[ -f "$zsh" ] && source "$zsh"; unset zsh

[ -f "$ZDOTDIR/.zshrc_extended" ] && source "$ZDOTDIR/.zshrc_extended"
