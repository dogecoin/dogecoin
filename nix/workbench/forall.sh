usage_forall() {
     usage "forall" "For-each-node iteration" <<EOF
    nodes SUBOP ARGS..    Iterate SUBOP for all N, where N is a valid node id.
                            N is passed as first argument to SUBOP,
                            thereby shifting the rest by 1.

EOF
}

forall() {
local op=${1:---help)}; shift

case "${op}" in
    nodes )
        local usage="USAGE: wb forall nodes SUBOP ARGS.."
        local subop=${1:?$usage}; shift
        local args=("$@")

        # for x in
        # $subop
        ;;

    * ) usage_forall;; esac
}
