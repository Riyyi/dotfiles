local ret_status="%(?:%{$fg_bold[green]%}➜ :%{$fg_bold[red]%}➜ )"

local usr_host="%{$fg[cyan]%}%n%{$reset_color%}@%{$fg[cyan]%}%m%{$reset_color%}"
local directory="%{$fg[green]%}%~%{$reset_color%}"
local arrow="%(?::%{$fg[red]%})➤%{$reset_color%}"

PROMPT='╭─${usr_host} ${directory} $(git_prompt_info)
╰─${arrow} '
RPROMPT='%t'

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg_bold[blue]%}(%{$reset_color%}%{$fg_bold[red]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$fg_bold[blue]%})%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY=" %{$fg[yellow]%}✗"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$reset_color"

ZSH_THEME_GIT_PROMPT_AHEAD=" ↑"
ZSH_THEME_GIT_PROMPT_BEHIND=" ↓"
ZSH_THEME_GIT_PROMPT_STASHED=" ●"
ZSH_THEME_GIT_PROMPT_UNMERGED="🖕🏻 "
ZSH_THEME_GIT_PROMPT_UNTRACKED=" %{$fg[yellow]%}⭑"
