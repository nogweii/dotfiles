# If I have installed grc on a system, alias a bunch of commands to use it:
if [[ "$TERM" != dumb ]] && (( $+commands[grc] )) ; then
  # Set alias for commands with known configuration with grc.
  local cmd cmds="
      arping blkid cc cvs df diff dig dnf docker du env fdisk findmnt free gcc
      getfacl getsebool gmake hping id ifconfig ioping iostat ip iptables
      ip6tables iwconfig journalctl last lastb lastlog ldap lsattr lsblk lsmod
      lsof lspci mount mtr netstat nmap nping ntpdate ping ping6 ps semanage
      showmount stat sysctl systemctl tcpdump traceroute traceroute6 tune2fs
      ulimit uptime vmstats w wdiff whois
  "

  for cmd in ${(z)cmds}; do
    [ "$cmd" = ';' ] && continue
    # check if the command actually exists on the system to avoid an alias to nothing
    if (( $+commands[$cmd] )) ; then
      alias $cmd="grc --colour=auto ${commands[$cmd]}"
    fi
  done

  unset cmds cmd
fi

# Want a list of commands grc supports?
# > for i in /usr/share/grc/conf.*; echo ${${i:t}#conf.}
