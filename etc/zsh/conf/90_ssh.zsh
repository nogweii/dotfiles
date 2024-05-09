# Test a bunch of locations where the SSH agent socket may be

# As of GNOME 3.28, the keyring agent wraps the upstream OpenSSH agent process
# and so I like it now. Set the AUTH_SOCK variable when it's around
if [ -S "${XDG_RUNTIME_DIR}/keyring/ssh" ]; then
    export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/keyring/ssh"
    SSH_AUTH_SOCK_SOURCE="gnome-keyring"

# user systemd managed ssh agent
elif [ -S "${XDG_RUNTIME_DIR}/ssh-agent.socket" ]; then
    export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/ssh-agent.socket"
    SSH_AUTH_SOCK_SOURCE="runtime-ssh-agent"

# ssh-agent-wsl installed to my home directory allows me to use Windows 10's
# ssh-agent in WSL
elif [ -n "${LXSS}" ]; then
    local ssh_agent_wsl="/mnt/c/Users/${WINUSER}/Local Applications/ssh-agent-wsl/ssh-agent-wsl"
    if [ -x "${ssh_agent_wsl}" ]; then
        eval $("${ssh_agent_wsl}" -q -r)
    fi
    SSH_AUTH_SOCK_SOURCE="windows-10-subsystem"

# Use keychain to launch a shared SSH agent across terminals when there is
# no gnome-keyring
elif [ -n "${commands[keychain]}" ]; then
    eval $(keychain --eval --quiet --noask --agents ssh --absolute --dir $XDG_RUNTIME_DIR/keychain --systemd)
    SSH_AUTH_SOCK_SOURCE="gentoo-keychain"
fi

# zstyle -e ':completion:*:(ssh|scp|sftp|ssh-copy-id):*' hosts 'reply=(${${${${(f)"$(<~/.ssh/known_hosts)"}:#[|0-9]*}%%\ *}%%,*} )'

# zstyle -e ':completion:*:hosts' hosts 'reply=(
#   ${=${=${=${${(f)"$(<~/.ssh/known_hosts(N))"}%%[#| ]*}//\]:[0-9]*/ }//,/ }//\[/ }
#   ${=${(f)"$(</etc/hosts(|)(N))"}%%*}
#   ${=${${${${(@M)${(f)"$(<~/.ssh/config)"}:#Host *}#Host }:#*\**}:#*\?*}}
# )'
# : ${(A)_etc_hosts:=${(s: :)${(ps:\t:)${${(f)~~"$(</etc/hosts)"}%%\#*}##[:blank:]#[^[:blank:]]#}}}


zstyle -e ':completion:*:hosts' hosts 'reply=(
  ${=${=${=${${(f)"$(cat {/etc/ssh/ssh_,~/.ssh/}known_hosts(|2)(N) 2> /dev/null)"}%%[#| ]*}//\]:[0-9]*/ }//,/ }//\[/ }
  ${=${(f)"$(cat /etc/hosts(|)(N))"}%%(\#${_etc_host_ignores:+|${(j:|:)~_etc_host_ignores}})*}
  ${=${${${${(@M)${(f)"$(cat ~/.ssh/config{,.d/*} 2> /dev/null)"}:#Host *}#Host }:#*\**}:#*\?*}}
)'
