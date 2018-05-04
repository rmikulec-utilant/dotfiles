#!/bin/bash
###############################################
# user_data.sh                     4 May 2018 #
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
    sed -i "1 s/$/ ${myHostName}/" /etc/hosts &> /dev/null
fi

export DEBIAN_FRONTEND=noninteractive
apt-get -y update

# Install emacs25
add-apt-repository -y ppa:kelleyk/emacs
apt-get -y update
apt-get -y install emacs25

# Install latest python3 and virtualenv
python_version=$(python3 --version)
if [ ".${python_version}" != ".Python 3.6.5" ]; then
    apt-get install -y build-essential
    apt-get install -y python-dev
    apt-get install -y python-setuptools
    apt-get install -y python-pip
    apt-get install -y python-smbus
    apt-get install -y libncurses-dev
    apt-get install -y libgdbm-dev
    apt-get install -y liblzma-dev
    apt-get install -y libc6-dev
    apt-get install -y zlib1g-dev
    apt-get install -y libsqlite3-dev
    apt-get install -y tk-dev
    apt-get install -y libreadline-dev
    apt-get install -y libssl-dev
    apt-get install -y openssl
    apt-get install -y libffi-dev
    apt-get install -y libbz2-dev

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
python3 -m pip install virtualenv

# Install tmux
apt-get -y install tmux

# Install other tools
apt-get -y install curl
apt-get -y install htop
apt-get -y install unzip
apt-get -y install libmysqlclient-dev
apt-get -y install python3-mysqldb

# Clean up, update, and upgrade
apt -y autoremove
apt-get update && apt-get -y -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" dist-upgrade

# Install zsh
apt-get -y install zsh
cp -p /etc/pam.d/chsh /etc/pam.d/chsh.backup
sed -ri "s|auth( )+required( )+pam_shells.so|auth sufficient pam_shells.so|" /etc/pam.d/chsh

python3 -m pip install jupyter_contrib_nbextensions
python3 -m pip install jupyter_nbextensions_configurator
python3 -m pip install autopep8
jupyter contrib nbextension install --system
jupyter nbextensions_configurator enable --system

# Non-root users that I might log in as:
non_root_users=(ubuntu erogers)
for user in ${non_root_users[@]}
do
    # Install iterm2 tweaks
    sudo -H -u ${user} bash -c "curl -sL https://iterm2.com/shell_integration/zsh -o ~/.iterm2_shell_integration.zsh"

    # Change shell to zsh and install oh-my-zsh
    sudo -H -u ${user} bash <<EOF
chsh -s $(which zsh)
curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh > ~/zsh_install.sh
sed -i '/printf "\${GREEN}"/,/printf "\${NORMAL}"/d' ~/zsh_install.sh && sed -i '/^\s*env zsh$/d' ~/zsh_install.sh
chmod u+x ~/zsh_install.sh
sh -c ~/zsh_install.sh && rm ~/zsh_install.sh
if [ -e ~/.zshrc.pre-oh-my-zsh ]; then
    cat ~/.zshrc.pre-oh-my-zsh >> ~/.zshrc
fi
cat >> ~/.zshrc <<EOL
source ~/.iterm2_shell_integration.zsh
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi
EOL
EOF

    # Copy over dotfiles from this repo
    sudo -H -u ${user} bash <<EOF
cp ./dotfiles/.gitconfig ~/
cp ./dotfiles/.bash_aliases ~/
cp ./dotfiles/.tmux.conf ~/
cp ./dotfiles/.emacs ~/.emacs
rsync -az ./dotfiles/.emacs.d ~/
EOF

done

unset DEBIAN_FRONTEND

exit 0
