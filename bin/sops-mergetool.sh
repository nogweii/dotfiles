#!/usr/bin/env bash

# Rename CLI parameters to friendlier names
# https://git-scm.com/docs/gitattributes#_defining_a_custom_merge_driver
base="$1"
local_="$2"
remote="$3"
merged="$4"

# Load the mergetool scripts
TOOL_MODE=merge
source "$(git --exec-path)/git-mergetool--lib"
mergetool=$(get_merge_tool)
setup_tool "${mergetool}"

# Create file names for decrypted contents
#   example_LOCAL_2823.yaml -> example_LOCAL_2823.decrypted.yaml
extension=".${base##*.}"
base_decrypted="${base/$extension/.decrypted$extension}"
local_decrypted="${local_/$extension/.decrypted$extension}"
remote_decrypted="${remote/$extension/.decrypted$extension}"
merged_decrypted="${base_decrypted/_BASE_/_MERGED_}"
backup_decrypted="${base_decrypted/_BASE_/_BACKUP_}"

# If anything goes wrong, then delete our decrypted files
handle_trap_exit() {
    rm "${base_decrypted}" || true
    rm "${local_decrypted}" || true
    rm "${remote_decrypted}" || true
    rm "${merged_decrypted}" || true
    rm "${backup_decrypted}" || true
}
trap handle_trap_exit EXIT

# Decrypt our file contents
sops --decrypt --show-master-keys "$base" > "$base_decrypted"
sops --decrypt --show-master-keys "$local_" > "$local_decrypted"
sops --decrypt --show-master-keys "$remote" > "$remote_decrypted"

# Create a merge-diff to compare against
git merge-file -p "$local_decrypted" "$base_decrypted" "$remote_decrypted" > "$merged_decrypted"
cp "$merged_decrypted" "$backup_decrypted"

# Set up variables for the mergetool
# https://github.com/git/git/blob/v2.8.2/mergetools/meld
# https://github.com/git/git/blob/v2.8.2/git-mergetool--lib.sh#L95-L111
LOCAL="$local_decrypted"
BASE="$base_decrypted"
REMOTE="$remote_decrypted"
MERGED="$merged_decrypted"
BACKUP="$backup_decrypted"

# Override `check_unchanged` with a custom script
check_unchanged() {
    # If the contents haven't changed, then fail
    if test "$MERGED" -nt "$BACKUP"; then
        return 0
    else
        exit 1
    fi
}

# Run the mergetool
run_merge_tool "${mergetool}" true

# Re-encrypt content
sops --encrypt "$merged_decrypted" > "$merged"
