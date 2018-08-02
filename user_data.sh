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

set -x

env

echo $PATH

which python
whereis python

which python3
whereis python3

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
python3 -m pip install virtualenv

# Install other tools
apt-get -y install tmux \
                   curl \
                   htop \
                   unzip \
                   libmysqlclient-dev \
                   python3-mysqldb

# # Clean up, update, and upgrade
# apt -y autoremove
# apt-get update && apt-get -y -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" dist-upgrade

# Jupyter NB Extensions
python3 -m pip install jupyter_contrib_nbextensions
python3 -m pip install jupyter_nbextensions_configurator
python3 -m pip install autopep8
jupyter contrib nbextension install --sys-prefix
jupyter nbextensions_configurator enable --sys-prefix
jupyter nbextension enable toc2/main
jupyter nbextension enable code_prettify/autopep8
nbdime extensions --enable

unset DEBIAN_FRONTEND

# Install zsh
apt-get -y install zsh
cp -p /etc/pam.d/chsh /etc/pam.d/chsh.backup
sed -ri "s|auth( )+required( )+pam_shells.so|auth sufficient pam_shells.so|" /etc/pam.d/chsh

# Users available for setup
full_user_list=($(cut -d: -f1 /etc/passwd))

# Users I would like to setup, if available
my_user_list=("ubuntu" "erogers" "dsw")

# Intersection of my_user_list with full_user_list:
users_to_setup=($(comm -12 <(printf '%s\n' "${full_user_list[@]}" | LC_ALL=C sort) <(printf '%s\n' "${my_user_list[@]}" | LC_ALL=C sort)))

for user in ${users_to_setup[@]}
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
set -o magicequalsubst
source ~/.iterm2_shell_integration.zsh
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi
if [ -f ~/.profile ]; then
   . ~/.profile
fi
export LC_CTYPE=en_US.UTF-8
EOL
sed -i "s/alias please='sudo'/# alias please='sudo'/" ~/.oh-my-zsh/lib/misc.zsh
EOF

    # Copy over dotfiles from this repo
    sudo -H -u ${user} bash <<EOF
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cp \${DIR}/.gitconfig ~/
cp \${DIR}/.bash_aliases ~/
cp \${DIR}/.tmux.conf ~/
cp \${DIR}/.emacs ~/.emacs
rsync -az \${DIR}/.ssh ~/
EOF

done

# Append to zshenv
mkdir -p /etc/zsh/
cat << RCFILE | sudo tee -a /etc/zsh/zshenv
export IP_ADDRESS="$(head -1 /etc/hosts | grep -o 'ip[[:digit:]-]\{1,\}' | sed -e 's|ip-||' | sed -e 's|-|.|g')"
alias launch_jupyter="rm -f .nohup.out ; touch .nohup.out ; ( nohup jupyter lab --NotebookApp.token='' --no-browser --ip=\${IP_ADDRESS} >> .nohup.out 2>&1 & ) ; \
                     ( tail -Fn0 .nohup.out & ) | grep -om1 '[[:space:]]\{1,\}http.*'"
RCFILE

exit 0
