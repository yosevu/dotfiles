
#
# Main prompt
#

# local host_name="%{$fg[cyan]%}λ"
local path_string="%{$fg[white]%}%1~"
local prompt_string="λ"


# Make prompt_string red if the previous command failed.
local return_status="%(?:%{$fg[green]%}$prompt_string:%{$fg[red]%}$prompt_string)"

PROMPT='${path_string} ${return_status} %{$reset_color%}'

#
# Right prompt
#

RPROMPT='$(git_super_status)'
ZSH_THEME_GIT_PROMPT_BRANCH="%{$fg[white]%}"
#ZSH_THEME_GIT_PROMPT_BRANCH="%{$fg_bold[white]%}"
