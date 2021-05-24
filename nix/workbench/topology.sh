usage_topology() {
     usage "topology" "Topology generation" <<EOF
    make PROFILE-JSON OUTDIR
                          Generate the full cluster topology, including:
                            - the Nixops/'cardano-ops' style topology
                            - the .dot and .pdf rendering

    topology density-map PROFILE-JSON TOPO-DIR
                          Generate the profile-induced map of node names
                            to pool density: 0, 1 or N (for dense pools)

    for-local-node N TOPO-DIR BASE-PORT
                          Given TOPO-DIR, containing the full cluster topology,
                            print topology for the N-th node,
                            while assigning it a local port number BASE-PORT+N

    for-local-observer PROFILE TOPO-DIR BASE-PORT
                          Given the profile and the full cluster topology,
                            print topology for the observer node,
                            while assigning it a local port number BASE-PORT+NODE-COUNT
EOF
}

topology() {
local op=${1:---help)}; shift

case "${op}" in
    make )
        local usage="USAGE:  wb topology make PROFILE-JSON OUTDIR"
        local profile_json=${1:?$usage}
        local outdir=${2:?$usage}

        local n_hosts=$(jq .composition.n_hosts "$profile_json")

        ## 0. Generate:
        #
        mkdir -p                 "$outdir"
        args=( --topology-output "$outdir"/topology-nixops.json
               --dot-output      "$outdir"/topology.dot
               --size             $n_hosts

               $(jq '.composition.locations
                    | map("--loc " + .)
                    | join(" ")
                    ' --raw-output "$profile_json")
             )
        cardano-topology "${args[@]}"

        ## 1. Render PDF:
        #
        neato -s120 -Tpdf \
              "$outdir"/topology.dot > "$outdir"/topology.pdf

        ## 2. Patch the nixops topology with the density information:
        #
        jq --slurpfile prof "$profile_json" '
           def nixops_topology_set_pool_density($topo; $density):
              $topo *
              { coreNodes:
                ( .coreNodes
                | map
                  ( . *
                    { pools:
                      (if .pools == null then 0 else
                       if .pools == 1    then 1 else
                          ([$density, 1] | max) end end)
                    }
                  )
                )
              };

           nixops_topology_set_pool_density(.; $prof[0].dense_pool_density)
           '   "$outdir"/topology-nixops.json |
        sponge "$outdir"/topology-nixops.json
        ;;

    density-map )
        local usage="USAGE:  wb topology density-map PROFILE-JSON TOPO-DIR"
        local profile_json=${1:?$usage}
        local topo_dir=${2:?$usage}

        args=(--slurpfile profile  "$profile_json"
              --slurpfile topology "$topo_dir"/topology-nixops.json
              --null-input
             )
        jq ' $topology[0] as $topo
           | $topo.coreNodes
           | map
             ({ key:   "\(.nodeId)"
              , value: ((.pools) // 0)
              })
           | from_entries
           ' "${args[@]}";;

    for-local-node )
        local usage="USAGE:  wb topology for-local-node N TOPO-DIR BASE-PORT"
        local i=${1:?$usage}
        local topo_dir=${2:?$usage}
        local basePort=${3:?$usage}

        args=(--slurpfile topology "$topo_dir"/topology-nixops.json
              --argjson   basePort $basePort
              --argjson   i         $i
              --null-input
             )
        jq 'def loopback_node_topology_from_nixops_topology($topo; $i):
              $topo.coreNodes[$i].producers                      as $producers
            | ($producers | map(ltrimstr("node-") | fromjson))   as $prod_indices
            | { Producers:
                ( $prod_indices
                | map
                  ({ addr:    "127.0.0.1"
                   , port:    ($basePort + .)
                   , valency: 1
                   }
                  ))
              };

            loopback_node_topology_from_nixops_topology($topology[0]; $i)
           ' "${args[@]}";;

    for-local-observer )
        local usage="USAGE:  wb topology for-local-observer PROFILE TOPO-DIR BASE-PORT"
        local profile=${1:?$usage}
        local topo_dir=${2:?$usage}
        local basePort=${3:?$usage}

        local prof=$(profile get $profile)

        args=(--slurpfile topology "$topo_dir"/topology-nixops.json
              --argjson   basePort $basePort
              --null-input
             )
        jq 'def loopback_observer_topology_from_nixops_topology($topo):
              [range(0; $topo.coreNodes | length)] as $prod_indices
            | { Producers:
                ( $prod_indices
                | map
                  ({ addr:    "127.0.0.1"
                   , port:    ($basePort + .)
                   , valency: 1
                   }
                  ))
              };

            loopback_observer_topology_from_nixops_topology($topology[0])
            ' "${args[@]}";;

    * ) usage_topology;; esac
}
