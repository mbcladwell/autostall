#!/bin/sh

# http://hayne.net/MacDev/Notes/unixFAQ.html#shellStartup
# We require Bash but for portability we'd rather not use /bin/bash or
# /usr/bin/env in the shebang, hence this hack.
if [ "x$BASH_VERSION" = "x" ]
then
    exec bash "$0" "$@"
fi

PAS=$'[ \033[32;1mPASS\033[0m ] '
ERR=$'[ \033[31;1mFAIL\033[0m ] '
WAR=$'[ \033[33;1mWARN\033[0m ] '
INF="[ INFO ] "
# ------------------------------------------------------------------------------
#+UTILITIES

_err()
{ # All errors go to stderr.
    printf "[%s]: %s\n" "$(date +%s.%3N)" "$1"
}

_msg()
{ # Default message to stdout.
    printf "[%s]: %s\n" "$(date +%s.%3N)" "$1"
}

_debug()
{
    if [ "${DEBUG}" = '1' ]; then
        printf "[%s]: %s\n" "$(date +%s.%3N)" "$1"
    fi
}

# Return true if user answered yes, false otherwise.
# $1: The prompt question.
prompt_yes_no() {
    while true; do
        read -rp "$1" yn
        case $yn in
            [Yy]*) return 0;;
            [Nn]*) return 1;;
            *) _msg "Please answer yes or no."
        esac
    done
}


updatesys()
{

sudo DEBIAN_FRONTEND=noninteractive apt-get --assume-yes update
sudo DEBIAN_FRONTEND=noninteractive apt-get  --assume-yes install gnupg git nscd postgresql-client
wget 'https://sv.gnu.org/people/viewgpg.php?user_id=15145' -qO - | sudo -i gpg --import -
wget 'https://sv.gnu.org/people/viewgpg.php?user_id=127547' -qO - | sudo -i gpg --import -
    
  
}

installguix()
{

git clone https://github.com/mbcladwell/labsolns.git


 sudo /home/admin/labsolns/scripts/guix-install-mod.sh
 ##guix pull
source /home/admin/.guix-profile/etc/profile 

 guix package --profile=/home/admin/aux-profile -i guile-dbi

## using guile-3.0.2    use the option "--allow-collisions" of "guix package"
guix install glibc-utf8-locales guile-lib postgresql@13.1 guile@3.0.2
sudo guix install glibc-utf8-locales
    
# After setting `PATH', run `hash guix' to make sure your shell refers to `/home/admin/.config/guix/current/bin/guix'.
#$ echo $PATH
#/home/admin/.config/guix/current/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/games:/usr/games
         


cd /home/admin/labsolns

## install of lnpg.scm
guix package --install-from-file=guix.scm


guile -e main -s labsolns.scm 127.0.0.1 5432 ln_admin welcome lndb init           

}



configure()
{

echo "export GUIX_PROFILE=\"/home/admin/.guix-profile\"" >> /home/admin/.bashrc
echo " . \"/home/admin/.guix-profile/etc/profile\"" >> /home/admin/.bashrc
echo "export LC_ALL=\"C\"" >> /home/admin/.bashrc
echo "export GUIX_LOCPATH=\"$HOME/.guix-profile/lib/locale\"" >> /home/admin/.bashrc


export GUIX_PROFILE="/home/admin/.guix-profile"
export LC_ALL="C"
export GUIX_LOCPATH="$HOME/.guix-profile/lib/locale" 

    
}


main()
{
    local tmp_path
    export DEBIAN_FRONTEND=noninteractive 
    _msg "Starting installation ($(date))"
    
    updatesys
    configure
    installguix
    
   
    _msg "${INF}cleaning up ${tmp_path}"
   

    _msg "${PAS}LIMS*Nucleus has successfully been installed!"
    _msg "${PAS}Please exit and log back in to initialize environment variables."
    source /home/admin/.bashrc
    # Required to source /etc/profile in desktop environments.
     _msg "${INF}Run 'nohup ~/run-limsn.sh' to start the application server in detached mode."
 }

## https://www.ubuntubuzz.com/2021/04/lets-try-guix.html

main "$@"
