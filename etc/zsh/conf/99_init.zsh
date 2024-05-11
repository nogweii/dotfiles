if [ -S "${XDG_RUNTIME_DIR}/podman/podman.sock" ]; then
    export DOCKER_HOST="unix://${XDG_RUNTIME_DIR}/podman/podman.sock"
fi

# Run fortune only if it's installed and we aren't connected to the machine via ssh
if [ -n "${commands[fortune]}" -a -z "$SSH_CONNECTION" ] ; then
    fortune -s # "Short" apothegms only
fi

[ -n "${commands[python3]}" ] && AUTOSWITCH_DEFAULT_PYTHON=python3
source ${ZDOTDIR}/plugins/autoswitch-virtualenv/autoswitch_virtualenv.plugin.zsh
AUTOSWITCH_SILENT='yes'
