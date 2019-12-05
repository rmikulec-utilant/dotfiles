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
	tmux \
	curl \
	htop \
	unzip

unset DEBIAN_FRONTEND

# Write to .bashrc as ubuntu:
cat <<-'EOF' | su ${NONROOT_USER} -c "tee -a ~/.bashrc"
   export PATH="/.pyenv/bin:$PATH"
   eval "$(pyenv init -)"
   eval "$(pyenv virtualenv-init -)"
EOF

# Run as ubuntu:
su ${NONROOT_USER} <<-EOF
	set -x

	# Change shell to zsh and install oh-my-zsh
	chsh -s $(which zsh)
	curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh > ~/zsh_install.sh
	sed -i '/printf "\${GREEN}"/,/printf "\${NORMAL}"/d' ~/zsh_install.sh && sed -i '/^\s*env zsh$/d' ~/zsh_install.sh
	chmod u+x ~/zsh_install.sh
	sh -c ~/zsh_install.sh && rm ~/zsh_install.sh
	if [ -e ~/.zshrc.pre-oh-my-zsh ]; then
	    cat ~/.zshrc.pre-oh-my-zsh >> ~/.zshrc
	fi
	cat >> ~/.zshrc <<-EOL
	    set -o magicequalsubst
	    if [ -f ~/.bash_aliases ]; then
	        . ~/.bash_aliases
	    fi
	    if [ -f ~/.profile ]; then
	       . ~/.profile
	    fi
	    export LC_CTYPE=en_US.UTF-8
	EOL
EOF

exit 0
