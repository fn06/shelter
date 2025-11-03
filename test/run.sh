#!/bin/bash

function reset_zpool {
  sudo zpool destroy shelter && sudo zpool create shelter /tmpfs/shelter.img  && sudo rm -rf ~/.cache/shelter/shelter/
}

function zpool_exists {
  sudo zpool history $1 > /dev/null
  if [[ $? -eq 0 ]]; then
    echo ""
  else
	echo "not found"
  fi
}

function init_zpool {
  check=$(zpool_exists "shelter")

  if [[ -n $check ]]; then
	  sudo mount -o size=5G -t tmpfs /mnt/tmpfs /tmpfs  
	  dd if=/dev/zero of=/tmpfs/shelter.img bs=5G count=10
	  sudo zpool create shelter /tmpfs/shelter.img
  fi
}

function run_shelter {
  sudo -E dune exec -- shelter main --file=$1
}

init_zpool
reset_zpool
run_shelter $1 




