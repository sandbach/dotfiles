if status is-interactive
    # Commands to run in interactive sessions can go here
end

starship init fish | source

zoxide init fish | source

fish_vi_key_bindings

# Emulates vim's cursor shape behavior
# Set the normal and visual mode cursors to a block
set fish_cursor_default block
# Set the insert mode cursor to a line
set fish_cursor_insert line
# Set the replace mode cursor to an underscore
set fish_cursor_replace_one underscore
# The following variable can be used to configure cursor shape in
# visual mode, but due to fish_cursor_default, is redundant here
set fish_cursor_visual block

function fish_user_key_bindings
    bind -M insert kj "if commandline -P; commandline -f cancel; else; set fish_bind_mode default; commandline -f backward-char force-repaint; end"
end

set -U fish_greeting ""

# fish_add_path /usr/local/texlive/2022/bin/x86_64-linux
fish_add_path /usr/local/texlive/2023/bin/x86_64-linux

fish_add_path ~/.emacs.d/bin
fish_add_path ~/.config/emacs/bin

set distro (cat /etc/issue | awk '{print $1}' | sed -r 's/\s+//g' | sed '2d')

if test "$distro" = "Pop!_OS"
	alias bat "batcat"
end

alias zz "z -"

alias lisp "rlwrap sbcl"

function take -d "Create a directory and set CWD"
    command mkdir $argv
    if test $status = 0
        switch $argv[(count $argv)]
            case '-*'

            case '*'
                cd $argv[(count $argv)]
                return
        end
    end
end

# funcsave bat zz

# see: https://dev.to/nimai/yet-another-guide-on-backing-up-dotfiles-3be6
function cfg 
	command git --git-dir=$HOME/.config/cfg/.git --work-tree=$HOME $argv
end

# for emacsclient:
set -gx ALTERNATE_EDITOR ""
set -gx EDITOR "emacsclient -c"
set -gx VISUAL "emacsclient -c -a emacs"

# function emacs # add -c to open in new frame
#     command emacsclient -a emacs $argv &
# end

function emacs
    if pidof emacsclient
        command emacsclient -a "emacs" $argv &
    else
        command emacsclient -c -a "emacs" $argv &
    end
end

set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME ; set -gx PATH $HOME/.cabal/bin $PATH /home/ajs/.ghcup/bin # ghcup-env
