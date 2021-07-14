#!/bin/bash
# Copyright (c) 2021 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

export sudo=''

DEPENDENCIES=(git wget make gpg)

force=false
# Get options and arguments
while :; do
    case $1 in
        # Verify
        -f|--force)
	    force=true
            ;;
	*)               # Default case: If no more options then break out of the loop.
             break
    esac
    shift
done

if [[ $force = false ]]; then
  while true; do
    read -p "Do you really want to install those dependencies: \"docker ${DEPENDENCIES[*]// / }\" ? (y/n) " yn
    case $yn in
        [Yy]* ) break;;
        [Nn]* ) exit;;
        * ) echo "Please answer yes or no.";;
    esac
  done
fi

package_manager=''
setup_docker_script=''
dependencies=''

echo "OS: $OSTYPE"

if [[ "$OSTYPE" == "linux-gnu"* ]]
then
  if command -v apt &> /dev/null; then
    export package_manager="apt"
    export package_manager_update="${package_manager} update"
    export package_manager_install="${package_manager} -y install"
  fi
  if command -v dnf &> /dev/null; then
    export package_manager="dnf"
    export package_manager_update="${package_manager} update"
    export package_manager_install="${package_manager} -y install"
  fi
  if command -v pacman &> /dev/null; then
    export package_manager="pacman"
    export package_manager_update="${package_manager} -Suy --noconfirm"
    export package_manager_install="${package_manager} -Suy --noconfirm"
  fi

  echo "Package Manager: $package_manager"
  if [[ -z "$package_manager" ]]; then
      echo "Unknown package manager..."
      exit 1
  fi

  if [[ ! $(whoami) = "root" ]]; then
    if [[ ! $(sudo -v && echo 1) ]]
    then
      echo "Sudo is not installed..."
      exit 1
    fi
    echo "Sudo is installed..."
    sudo='sudo '
  fi

  export setup_docker_script="setup/package_manager/setup_${package_manager}_docker.sh"
  ./setup/setup_linux.sh

elif [[ "$OSTYPE" == "darwin"* ]]
then
  export package_manager_install="brew install"
  export package_manager_update="brew update"
  export setup_docker_script="setup/package_manager/setup_darwin_docker.sh"
  ./setup/setup_darwin.sh
else
  echo "Unknown OS"
  exit 1
fi

echo "Checking..."

if ! command -v docker &> /dev/null
then
   ./${setup_docker_script}
   if ! command -v docker &> /dev/null
   then
     echo "Docker not properly installed..."
     exit 1
   fi
fi

for dependency in "${DEPENDENCIES[@]}"; do
  echo "Checking dependency: $dependency..."
  if ! command -v $dependency &> /dev/null
  then
    echo "$dependency: ko"
    dependencies="$dependencies $dependency"
  else
    echo "$dependency: ok"
  fi
done

echo "Dependencies to be installed: $dependencies"
echo "$package_manager_install $dependencies"
if [[ ! -z $dependencies ]]
then
  $package_manager_install $dependencies
fi


for dependency in "${DEPENDENCIES[@]}"; do
  echo "Checking dependency: $dependency..."
  if ! command -v $dependency &> /dev/null
  then
    echo "$dependency not properly installed..."
    exit 1
  fi
done

echo "SUCCESS: INIT DONE!"