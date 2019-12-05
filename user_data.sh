#!/bin/bash
###############################################
# user_data                                   #
#                                             #
#    This script, called by the EC2 user data #
# at instance first launch, sets up the dev   #
# environment. It is run as root, and will:   #
# * Update the apt-get repository listings    #
# * Install zsh                               #
# * Install Python3 and pip if needed         #
# * Install Emacs25                           #
# * Install pyenv and pyenv-virtualenv        #
# * Install tmux                              #
# * Install curl, htop, & unzip               #
#                                             #
###############################################

set -euxo pipefail

NONROOT_USER=ubuntu

export DEBIAN_FRONTEND=noninteractive
apt-get -y update

# Install zsh
apt-get -y install zsh
cp -p /etc/pam.d/chsh /etc/pam.d/chsh.backup
sed -ri "s|auth( )+required( )+pam_shells.so|auth sufficient pam_shells.so|" /etc/pam.d/chsh

# Install emacs25
apt-get -y install emacs25

# Install python3 & pip
apt-get install -y \
	python3 \
	python3-pip

# Install pyenv
curl https://pyenv.run | bash

# Install other tools
apt-get -y install \
	curl \
	docker \
	htop \
	tmux \
	unzip

unset DEBIAN_FRONTEND

exit 0
