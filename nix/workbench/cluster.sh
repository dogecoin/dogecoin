usage_cluster() {
     usage "cluster" "Cluster operation" <<EOF
    deploy-profile NAME   Deploy profile and start the cluster.

EOF
}

cluster() {
local op=${1:---help)}; shift

case "${op}" in
    deploy-profile | deploy )
        local usage="USAGE: wb cluster deploy-profile SUBOP ARGS.."
        local subop=${1:?$usage}; shift
        local args=("$@")

        # for x in
        # $subop
        ;;

    * ) usage_cluster;; esac
}
