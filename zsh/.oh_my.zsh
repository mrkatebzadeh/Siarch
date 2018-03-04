# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="powerlevel9k/powerlevel9k"

#POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(time context dir virtualenv vcs)
#POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status history)

POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(root_indicator context dir_writable dir vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(command_execution_time background_jobs status time ssh)

POWERLEVEL9K_SHORTEN_STRATEGY=truncate_middle
POWERLEVEL9K_SHORTEN_DELIMITER="*"
POWERLEVEL9K_PROMPT_ON_NEWLINE=true
POWERLEVEL9K_TIME_FORMAT='%D{%H:%M:%S}'
POWERLEVEL9K_VIRTUALENV_BACKGROUND='150'  # Greenish
POWERLEVEL9K_SHORTEN_DIR_LENGTH=1
POWERLEVEL9K_HOST_ICON="" # 
POWERLEVEL9K_ROOT_ICON=$''  # 

POWERLEVEL9K_SSH_FOREGROUND="$DEFAULT_FOREGROUND"
POWERLEVEL9K_SSH_FOREGROUND="yellow"
POWERLEVEL9K_SSH_BACKGROUND="$DEFAULT_BACKGROUND"
POWERLEVEL9K_SSH_BACKGROUND="$(( $DEFAULT_BACKGROUND + 2 ))"
POWERLEVEL9K_SSH_BACKGROUND="$(( $DEFAULT_BACKGROUND - 2 ))"
POWERLEVEL9K_SSH_ICON="\uF489"  # 

POWERLEVEL9K_HOST_LOCAL_FOREGROUND="$DEFAULT_FOREGROUND"
POWERLEVEL9K_HOST_LOCAL_BACKGROUND="$DEFAULT_BACKGROUND"
POWERLEVEL9K_HOST_REMOTE_FOREGROUND="$DEFAULT_FOREGROUND"
POWERLEVEL9K_HOST_REMOTE_BACKGROUND="$DEFAULT_BACKGROUND"

POWERLEVEL9K_HOST_ICON_FOREGROUND="$DEFAULT_FOREGROUND"
POWERLEVEL9K_HOST_ICON_BACKGROUND="$DEFAULT_BACKGROUND"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(autojump command-not-found  gnu-utils history-substring-search pip python zsh-autosuggestions zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

