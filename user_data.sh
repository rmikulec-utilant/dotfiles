#!/bin/bash
###############################################
# user_data                        4 May 2018 #
#                                   Ed Rogers #
#                                             #
#    This script, called by the EC2 user data #
# at instance first launch, sets up the dev   #
# environment. It is run as root, and will:   #
# * Fix a known ubuntu issue with /etc/hosts  #
# * Copy over some dotfiles from this repo    #
# * Install Python 3.6.5                      #
# * Install Emacs25                           #
# * Install shell integration for iterm2      #
# * Install virtualenv                        #
# * Install matplotlib                        #
# * Install tmux                              #
# * Install curl, htop, unzip, & mysqlclient  #
# * Change the default shell to zsh           #
# * Install the oh-my-zsh theme               #
# * Clean up                                  #
#                                             #
###############################################

#  First, fix the problem of AmFam ubuntu EC2 instances complaining
# about a strange entry in the /etc/hostname file every time sudo is
# used. This problem doesn't cause any real issues, but can be
# distracting and is easily fixed.
hosts_file_fixed=$(grep -c $(cat /etc/hostname) /etc/hosts)
if [ $hosts_file_fixed -eq 0 ]; then
    myHostName=$(cat /etc/hostname)
    cp -p /etc/hosts ./hosts.new
    sed -i "1 s/\$/ ${myHostName}/" ./hosts.new
    cat ./hosts.new > /etc/hosts
    rm ./hosts.new
fi

export DEBIAN_FRONTEND=noninteractive
apt-get -y update

# Install emacs25
add-apt-repository -y ppa:kelleyk/emacs
apt-get -y update
apt-get -y install emacs25

# Check if python version is >= 3.6.5
new_enough_version=$(python3 -c "import sys; print(sys.version_info >= (3,6,5))")
if [ ".${new_enough_version}" != ".True" ]; then
    # If not, install python3 from source
    apt-get install -y build-essential \
                       python-dev \
                       python-setuptools \
                       python-pip \
                       python-smbus \
                       libncurses-dev \
                       libgdbm-dev \
                       liblzma-dev \
                       libc6-dev \
                       zlib1g-dev \
                       libsqlite3-dev \
                       tk-dev \
                       libreadline-dev \
                       libssl-dev \
                       openssl \
                       libffi-dev \
                       libbz2-dev
    wget -nv https://www.python.org/ftp/python/3.6.5/Python-3.6.5.tgz
    tar xzf Python-3.6.5.tgz
    cd Python-3.6.5/
    ./configure
    make
    make install
    cd -
    rm -rf Python-3.6.5 Python-3.6.5.tgz
    python3 -m pip install --upgrade pip
fi

# Install virtualenv
python3 -m pip install -U pip
python3 -m pip install virtualenv

# Install other tools
apt-get -y install tmux \
                   curl \
                   htop \
                   unzip \
                   libmysqlclient-dev \
                   python3-mysqldb

# Update npm
npm install npm@latest -g

# Clean up, update, and upgrade
apt -y autoremove
apt-get update && apt-get -y -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" dist-upgrade

unset DEBIAN_FRONTEND

# Install zsh
apt-get -y install zsh
cp -p /etc/pam.d/chsh /etc/pam.d/chsh.backup
sed -ri "s|auth( )+required( )+pam_shells.so|auth sufficient pam_shells.so|" /etc/pam.d/chsh

exit 0
