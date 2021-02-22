#!/bin/bash
###############################################
# user_data                                   #
#                                             #
#    This script, called by the EC2 user data #
# at instance first launch, sets up the dev   #
# environment. It is run as root.             #
#                                             #
###############################################

set -euxo pipefail

NONROOT_USER=ubuntu

export DEBIAN_FRONTEND=noninteractive

apt-get -y update
apt-get -y install tmux

# Install add-jupyter-kernel script
cp -p add-jupyter-kernel /usr/local/bin/add-jupyter-kernel
chown ${NONROOT_USER}: /usr/local/bin/add-jupyter-kernel

unset DEBIAN_FRONTEND

exit 0
