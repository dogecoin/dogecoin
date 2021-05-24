default_cacheDir="$HOME"/.cache/cardano-workbench
default_basePort=30000

usage_env() {
     usage "env" "Environment setup" <<EOF
    compute-config [ENV-OPTS..]
                          Compute the environment configuration JSON.
                          The following environment config options are defined:

        --cache-dir DIR      Set the cache directory;  Defaults to $default_cachedir
        --base-port PORTNO   Set base port number;  Defaults to $default_basePort
        --stagger-ports      Whether to allocate different ports for each node;
                               Defaults to no port staggering

EOF
}

env() {
local op=${1:---help)}; shift

case "${op}" in
    compute-config )
        local usage="USAGE: wb run OPTS.. print-env-config"

        local v=(
            --arg     cacheDir     "$default_cacheDir"
            --argjson basePort     "$default_basePort"
            --argjson staggerPorts 'false'
            )

        while test $# -gt 0; do case "$1" in
           --cache-dir )     v=(--arg     cacheDir      "$2"  "${v[@]}"); shift;;
           --base-port )     v=(--argjson basePort       $2   "${v[@]}"); shift;;
           --stagger-ports ) v=(--argjson staggerPorts  true "${v[@]}");;
           * ) fatal "wb run print-env-config: unknown args: $*";; esac
           shift; done

        jq '{ cacheDir:      $cacheDir
            , basePort:      $basePort
            , staggerPorts:  $staggerPorts
            }' --null-input "${v[@]}";;

    * ) usage_env;; esac
}
