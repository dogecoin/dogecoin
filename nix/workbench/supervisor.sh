set -euo pipefail

. $(dirname "$0")/lib.sh

usage_supervisor() {
     usage "supervisor" "Backend:  manages a local cluster using 'supervisord'" <<EOF
    is-running       Test if 'supervisord' is running

    get-node-socket-path STATE-DIR
                     Given a state dir, print the default node socket path
                       for 'cardano-cli'
EOF
}

op=${1:?$(usage_supervisor)}; shift

case "$op" in
    name )
        echo 'supervisor';;

    is-running )
        test "$(sleep 0.5s; netstat -pltn 2>/dev/null | grep ':9001 ' | wc -l)" != "0"
        ;;

    get-node-socket-path )
        usage="USAGE: wb supervisor $op STATE-DIR"
        state_dir=${1:?$usage}

        echo -n $state_dir/node-0/node.socket
        ;;

    record-extended-env-config )
        usage="USAGE: wb supervisor $op ENV-JSON [ENV-CONFIG-OPTS..]"
        env_json=${1:?$usage}

               port_shift_ekg=200
        port_shift_prometheus=300
        while test $# -gt 0
        do case "$1" in
               --port-shift-ekg )        port_shift_ekg=$2; shift;;
               --port-shift-prometheus ) port_shift_prometheus=$2; shift;;
               --* ) msg "FATAL:  unknown flag '$1'"; usage_supervisor;;
               * ) break;; esac; shift; done

        env_json="$env_json"
        args=(
            --argjson port_shift_ekg        "$port_shift_ekg"
            --argjson port_shift_prometheus "$port_shift_prometheus"
        )
        jq_fmutate "$env_json" '. *
          { port_shift_ekg:        $port_shift_ekg
          , port_shift_prometheus: $port_shift_prometheus
          }
        ' "${args[@]}"
        ;;

    describe-run )
        usage="USAGE: wb supervisor $op RUN-DIR"
        dir=${1:?$usage}

        basePort=$(jq .basePort "$dir"/env.json)
        port_ekg=$((       basePort+$(jq .port_shift_ekg        "$dir"/env.json)))
        port_prometheus=$((basePort+$(jq .port_shift_prometheus "$dir"/env.json)))

        cat <<EOF
  - EKG URL (node-0):        http://localhost:$port_ekg/
  - Prometheus URL (node-0): http://localhost:$port_prometheus/metrics
EOF
        ;;

    pre-run-hook )
        usage="USAGE: wb supervisor $op RUN-DIR"
        dir=${1:?$usage}

        wb backend assert-stopped

        if test -e "$dir" -a ! -L "$dir"
        then echo "workbench ERROR:  state directory exists, but is not a symlink -- please remove it or choose another:  $dir"; exit 1; fi
        ;;

    start-run )
        usage="USAGE: wb supervisor $op RUN-DIR"
        dir=${1:?$usage}; shift

        supervisorConf=
        while test $# -gt 0
        do case "$1" in
               --supervisor-conf ) supervisorConf=$2; shift;;
               --* ) msg "FATAL:  unknown flag '$1'"; usage_supervisor;;
               * ) break;; esac; shift; done

        test -r "$supervisorConf" ||
            fatal "supervisor start-run requires the --supervisor-conf FILE option."

        mkdir -p              "$dir"/supervisor
        cp -f $supervisorConf "$dir"/supervisor/supervisord.conf
        supervisord --config  "$dir"/supervisor/supervisord.conf $@

        if test ! -v CARDANO_NODE_SOCKET_PATH
        then export  CARDANO_NODE_SOCKET_PATH=$($0 get-node-socket-path "$dir")
        fi
        while test ! -S $CARDANO_NODE_SOCKET_PATH
        do msg "supervisor:  waiting 5 seconds for $CARDANO_NODE_SOCKET_PATH to appear.."
           sleep 5
        done

        msg "supervisor:  pid file:  $dir/supervisor/supervisord.pid"
        pstree -Ap "$(cat "$dir"/supervisor/supervisord.pid)" |
            grep 'cabal.*cardano-node' |
            sed -e 's/^.*-+-{\?cardano-node}\?(\([0-9]*\))$/\1/' \
                > "$dir"/supervisor/cardano-node.pids
        ;;

    * ) usage_supervisor;; esac
