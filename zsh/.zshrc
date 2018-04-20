# files to source in priority
if [ -z "$TMUX" ]
then
    # exec tmux 
fi
source $HOME/.oh_my.zsh
source /usr/share/zsh-theme-powerlevel9k/powerlevel9k.zsh-theme
# load zsh config files
config_files=($HOME/.zsh/**/*.zsh(N))
for file in ${config_files}
do
  source $file
done

