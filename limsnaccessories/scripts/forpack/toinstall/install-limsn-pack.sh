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
    
    echo "Set up local database?[y|N]"
    read INIT_DB
    
}



installln()
{

 wget https://github.com/labsolns/labsolns/releases/download/v0.1.0p/limsn-0.1.0-pack.tar.gz

   tar xf ./limsn-0.1.0-pack.tar.gz
   
   export PATH="$HOME/bin${PATH:+:}$PATH"
   init-limsn-pack.sh	
	
    sudo sed -i "s/host.name = 127.0.0.1/host.name = $IPADDRESS/" $HOME/.config/limsn/artanis.conf
    sudo sed -i "s/maxplates = 100/maxplates = $MAXPLATES/"  $HOME/.config/limsn/artanis.conf 
      

 
}


initdb()
{
    _msg "configuring db"

sudo DEBIAN_FRONTEND=noninteractive apt-get  --assume-yes update
sudo DEBIAN_FRONTEND=noninteractive apt-get  --assume-yes install postgresql

 PGMAJOR=$(eval "ls /etc/postgresql")
 PGHBACONF="/etc/postgresql/$PGMAJOR/main/pg_hba.conf"
 sudo sed -i 's/host[ ]*all[ ]*all[ ]*127.0.0.1\/32[ ]*md5/host    all        all             127.0.0.1\/32        trust/' $PGHBACONF
 PGCONF="/etc/postgresql/$PGMAJOR/main/postgresql.conf"
 sudo sed -i 's/\#listen_addresses =/listen_addresses =/' $PGCONF
 eval "sudo pg_ctlcluster $PGMAJOR main restart"
    
}

loaddb()
{

load-pg.sh postgres

}

startln()
{
    
    start-limsn.sh
    
}


main()
{
    local tmp_path
    welcome
    export DEBIAN_FRONTEND=noninteractive 
    _msg "Starting installation ($(date))"

    query
    
    if [ "$INIT_DB" == "y" ]; then initdb; fi 
    
    installln
    
    if [ "$INIT_DB" == "y" ]; then loaddb; fi  
##    if [ "$INIT_DB" == "y" ]; then startln; fi  
    
    
  
    _msg "${PAS}LIMS*Nucleus has successfully been installed!"
    _msg "Restart terminal to source ~/.bashrc and set environment variables."
    _msg "${INF}Run 'nohup ~/start-limsn.sh &' to start the server in detached mode."
 }

main "$@"

