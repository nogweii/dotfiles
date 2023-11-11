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

# user systemd managed GnuPG SSH agent compatible
elif [ -S "${XDG_RUNTIME_DIR}/gnupg/S.gpg-agent.ssh" ]; then
    export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/gnupg/S.gpg-agent.ssh"
    SSH_AUTH_SOCK_SOURCE="runtime-gpg-agent"

# old GnuPG agent location
elif [ -S ~/.gnupg/S.gpg-agent.ssh ]; then
    export SSH_AUTH_SOCK=~/.gnupg/S.gpg-agent.ssh
    SSH_AUTH_SOCK_SOURCE="legacy-gpg-agent"

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
    eval $(keychain --eval --quiet --timeout 120 --noask --agents ssh --absolute --dir $XDG_CACHE_HOME/keychain --host localhost)
    SSH_AUTH_SOCK_SOURCE="gentoo-keychain"
fi

