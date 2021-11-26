# Kalelzar's traumatic Z Shell config

# Enable colors and change prompt:
autoload -U colors && colors	# Load colors
setopt prompt_subst

vterm_prompt(){
    vterm_printf "51;A$(whoami)@$(uname -n):$(pwd)";
}

precmd(){
    # Show some task stats
    #getTaskInfoa
    # Write some info to terminal title.
    # This is seen when the shell prompts for input.
    EXIT_CODE="$?"
    print -Pn "\e]0;%~\a"
    PS1="$(powerline -r .zsh shell aboveleft -w $(tput cols) --last-exit-code $EXIT_CODE)"'%{$(vterm_prompt)%}'
    RPROMPT="$(powerline -r .zsh shell right)"
}

postcmd(){
    print -Pn "\e]0;%~ - vterm\a"
}

vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}


if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi


# Convenience

setopt autocd		# Automatically cd into typed directory.

# History in cache directory:
HISTSIZE=10000
SAVEHIST=10000

# Load aliases and shortcuts if existent.
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shortcutrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shortcutrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/aliasrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/aliasrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/zshnameddirrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/zshnameddirrc"

# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.

#emacs mode
bindkey -e

bindkey '^[[A' up-line-or-search
bindkey '^[[B' down-line-or-search
bindkey '^[^[[C' emacs-forward-word
bindkey '^[^[[D' emacs-backward-word

bindkey -s '\C-x\C-z' '%-^M'

#No idea what any of this does
bindkey '\M-i' reverse-menu-complete
#No idea what this does either
bindkey '\M-e' expand-cmd-path


#No idea what this does
#Amended: It does some fancy stuff I don't really understand
#So the above still applies
bindkey '\C-x\C-n' accept-and-infer-next-history

bindkey '\C-w' kill-region
bindkey '\M-p' complete-word

#Copy stuff
copy-region-as-kill-deactivate-mark () {
    zle copy-region-as-kill
    zle set-mark-command -n -1
}

zle -N copy-region-as-kill-deactivate-mark

bindkey '\M-w' copy-region-as-kill-deactivate-mark

#Change directory with fzf
bindkey -s '\C-f' 'cd "$(dirname "$(fzf)")"\n'
bindkey -s '\C-s' 'eval "$(fc -l -n 0 | sort -u | fzf)"\n'

# Edit line in editor (Default: emacs) with C-x C-e:
autoload edit-command-line
zle -N edit-command-line
bindkey '\C-x\C-e' edit-command-line




export KEYTIMEOUT=1

# echo -ne '\e[5 q' # Use beam shape cursor on startup.
# preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.


#source "$XDG_CONFIG_HOME/zsh/notes"

# Write command and args to terminal title.
# This is seen while the shell waits for a command to complete.
preexec() {
    printf "\033]0;%s\a" "$1"
}

# Load syntax highlighting; should be last.
source /usr/share/zsh/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh 2>/dev/null
