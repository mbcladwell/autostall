#!/bin/sh

# We require Bash but for portability we'd rather not use /bin/bash or
# /usr/bin/env in the shebang, hence this hack.
if [ "x$BASH_VERSION" = "x" ]
then
    exec bash "$0" "$@"
fi

# set -e
# [ "$UID" -eq 0 ] || { echo "This script must be run as root."; exit 1; }


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

welcome()
{
    cat<<"EOF"

 _______________________  |  _ |_  _  _ _ _|_ _  _         
|O O O O O O O O O O O O| |_(_||_)(_)| (_| | (_)| \/       
|O O O O O O 1 O O O O O|                         /        
|O O O O O O O O O O O O|  /\    _|_ _  _ _  _ _|_. _  _   
|O O O O O O O O O O O O| /~~\|_| | (_)| | |(_| | |(_)| |  
|O O 1 O O O O O 1 O 1 O|  _                               
|O O O O O O O O O O O O| (  _ |   _|_. _  _  _            
|O O O 1 O O O O O O O O| _)(_)||_| | |(_)| |_)    
|O O O O O O O O O O O O|
 -----------------------  info@labsolns.com

This script installs LIMS*Nucleus on your system

http://www.labsolns.com

EOF
    echo -n "Press return to continue..."
    read -r
}

query()
{
    echo Enter IP address:
    read IPADDRESS
    
    echo Maximum number of plates per plate set:
    read MAXPLATES
    
}

updatesys()
{
    sudo DEBIAN_FRONTEND=noninteractive apt-get --assume-yes update
    sudo DEBIAN_FRONTEND=noninteractive apt-get --assume-yes upgrade
 ##   sudo DEBIAN_FRONTEND=noninteractive apt-get  --assume-yes install gnupg git nscd postgresql  postgresql-contrib nano
    sudo DEBIAN_FRONTEND=noninteractive apt-get  --assume-yes install gnupg git nscd nano
    
}


guixinstall()
{
    wget 'https://sv.gnu.org/people/viewgpg.php?user_id=15145' -qO - | sudo -i gpg --import -
    wget 'https://sv.gnu.org/people/viewgpg.php?user_id=127547' -qO - | sudo -i gpg --import -  

    sudo ./guix-install-mod.sh

  ## using guile-3.0.2
    guix install glibc-utf8-locales
    GUIX_PROFILE="$HOME/.guix-profile"
    . $GUIX_PROFILE/etc/profile
    sudo guix install glibc-utf8-locales
    export GUIX_LOCPATH="$HOME/.guix-profile/lib/locale"
    guix pull
    hash guix
    ##guix package -u
    
    . $HOME/.config/guix/current/etc/profile
             
    . $HOME/.guix-profile/etc/profile
    ##  guix package --install-from-file=$HOME/ln11/limsn.scm
    guix package -i limsn
    . $HOME/.guix-profile/etc/profile

    init-limsn.sh	
	
    sudo sed -i "s/host.name = 127.0.0.1/host.name = $IPADDRESS/" $HOME/.config/limsn/artanis.conf
    sudo sed -i "s/maxplates = 100/maxplates = $MAXPLATES/"  $HOME/.config/limsn/artanis.conf 
      
}

initdb()
{
    _msg "configuring db"

install-pg-aws.sh
    
}


makepubkey()
{
_msg "Loading public key"
  
  
touch ~/public.key

echo "-----BEGIN PGP PUBLIC KEY BLOCK-----" >> ~/public.key
echo " " >> ~/public.key
echo "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=LV2i" >> ~/public.key
echo " " >> ~/public.key
echo "-----END PGP PUBLIC KEY BLOCK-----" >> ~/public.key

sudo gpg --import ./public.key
rm ./public.key

}

makechannelsscm()
{
_msg "Making channels.scm"
  
 mkdir -p ./.config/guix
 touch ./.config/guix/channels.scm
     
 
 echo "(use-modules (guix ci)
     (guix channels))
 (list (channel
        (name 'guix)
        (url \"https://git.savannah.gnu.org/git/guix.git\")
        (branch \"master\")
        (commit
          \"5b71034fe81c2639bb7a9121146ddc551f6067f9\")
        (introduction
          (make-channel-introduction
            \"9edb3f66fd807b096b48283debdcddccfea34bad\"
            (openpgp-fingerprint
              \"BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA\"))))
      (channel
        (name 'labsolns)
        (url \"https://github.com/mbcladwell/labsolns\")
        (branch \"master\")
        (introduction
          (make-channel-introduction
            \"1d6418bca78f0ea76e258a4842bfbe46cf76cbba\"
            (openpgp-fingerprint
              \"E709 94D1 9CB0 FE2B CAC4  9E54 0BF8 F292 4D2B 1944\")))))"  >> ~/.config/guix/channels.scm

}

main()
{
    local tmp_path
    welcome
    export DEBIAN_FRONTEND=noninteractive 
    _msg "Starting installation ($(date))"

    query
    updatesys
    makechannelsscm
    makepubkey
    guixinstall
   initdb  
    
    _msg "${INF}cleaning up ${tmp_path}"
    rm -r "${tmp_path}"

    _msg "${PAS}LIMS*Nucleus has successfully been installed!"

    # Required to source /etc/profile in desktop environments.
    _msg "${INF}Run 'nohup ~/start-limsn.sh &' to start the server in detached mode."
 }

main "$@"

