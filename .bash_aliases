alias e="emacs -nw"
alias t="tmux attach || tmux"
alias uudr="sudo apt-get update && sudo DEBIAN_FRONTEND=noninteractive apt-get -y -o Dpkg::Options::=\"--force-confdef\" -o Dpkg::Options::=\"--force-confold\" dist-upgrade && sudo reboot now"

function iterm2_print_user_vars() {
  iterm2_set_user_var gitBranch $((git branch 2> /dev/null) | grep \* | cut -c3-)
}

# IP address detection
if [ ".${IP_ADDRESS}" == "." ]; then
  public_ip="$(curl -s http://instance-data/latest/meta-data/public-ipv4 | grep -Eo '[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}')"
  private_ip="$(curl -s http://instance-data/latest/meta-data/local-ipv4 | grep -Eo '[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}')"
  if [ ".${public_ip}" != "." ]; then
    IP_ADDRESS=${public_ip};
  else
    IP_ADDRESS=${private_ip};
  fi;
  export IP_ADDRESS
fi
export iterm2_hostname=${IP_ADDRESS}
