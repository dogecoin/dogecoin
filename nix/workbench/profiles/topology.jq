## Cluster composition is an extract from the topology,
## that classifies nodes into BFT, regular pools and dense pools,
## based on the 'pools' field.
##
## Testable with:
##
##   jq -n 'include "composition" { search: "nix/supervisord-cluster/profiles" }; topology_composition({ coreNodes: { bft1: { pools: 0 } } })'
##
def topology_composition($topo):
    $topo
  | (.Producers // .coreNodes // {})
  | to_entries
  | map (.value.pools // 0)
  | length                           as $n_hosts
  | map (select (. == 0))            as $bfts
  | map (select (. != 0))            as $pools
  | ($pools | map (select (. == 1))) as $singular_pools
  | ($pools | map (select (.  > 1))) as $dense_pools
  | ($singular_pools | length)       as $n_singular_hosts
  | { n_bft_hosts:      ($bfts           | length)
    , n_singular_hosts: ($singular_pools | length)
    , n_dense_hosts:    ($dense_pools    | length)
    };
