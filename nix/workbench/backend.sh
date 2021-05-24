usage_backend() {
     usage "backend" "Abstract over cluster backend operations" <<EOF
    is-running       Test if cluster is running

    get-node-socket-path RUNDIR
                     Given a run directory, print the node socket path
                       for 'cardano-cli'

    start RUNDIR     Start an allocated run

    record-extended-env-config ENV-JSON [ENV-CONFIG-OPTS..]
                     Extend the environment JSON file with backend-specific
                       environment config

    assert-is BACKEND-NAME
                     Check that the current backend is as expected

    assert-stopped   Assert that cluster is not running
EOF
}

backend() {
local op=${1:-$(usage_backend)} # No need to shift -- backends will use the op.

case "${op}" in
    is-running )                 $WORKBENCH_BACKEND "$@";;
    get-node-socket-path )       $WORKBENCH_BACKEND "$@";;
    wait-for-local-node-socket ) $WORKBENCH_BACKEND "$@";;
    record-extended-env-config ) $WORKBENCH_BACKEND "$@";;
    describe-run )               $WORKBENCH_BACKEND "$@";;
    pre-run-hook )               $WORKBENCH_BACKEND "$@";;
    start-run )                  $WORKBENCH_BACKEND "$@";;

    assert-is )
        local usage="USAGE: wb run $op BACKEND-NAME"
        local name=${2:?$usage}

        ## Check the backend echoes own name:
        local actual_name=$($WORKBENCH_BACKEND name)
        if test "$actual_name" != "$name"
        then fatal "Workbench is broken:  '$WORKBENCH_BACKEND name' returned:  '$actual_name'"; fi
        ;;

    assert-stopped )
        backend is-running &&
          fatal "backend reports that cluster is already running. Please stop it first!" ||
          true
        ;;

    * ) usage_backend;; esac
}
