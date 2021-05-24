#!/usr/bin/env nix-shell
#!nix-shell -A devops ../shell.nix -i bash
nix flake update --update-input iohkNix
