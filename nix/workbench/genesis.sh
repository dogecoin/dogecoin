usage_genesis() {
     usage "genesis" "Genesis" <<EOF
    prepare [--force] CACHEDIR PROFILE-JSON TOPO-DIR OUTDIR
                     Prepare a genesis cache entry profile.
                       Cache entry regeneration can be --force 'd

    profile-cache-key-input PROFILE-JSON
                     Output effective profile parameters factoring into the
                       genesis cache key

    profile-cache-key PROFILE-JSON
                     Output a genesis cache key for the specified profile

    actually-genesis PROFILE-JSON TOPO-DIR DIR
                     (DEV) Internal procedure to actually generate genesis

    finalise-cache-entry PROFILE-JSON DIR
                     (DEV) Update a genesis cache entry to the given profile
EOF
}

genesis() {
local op=${1:-$(usage_genesis)}; shift

case "${op}" in
    prepare )
        local usage="USAGE:  wb genesis prepare [--force] CACHEDIR PROFILE-JSON TOPO-DIR OUTDIR"

        local regenesis_causes=()
        while test $# -gt 0
        do case "$1" in
               --force ) regenesis_causes+=('has--force');;
               * ) break;; esac; shift; done

        local cachedir=${1:?$usage}
        local profile_json=${2:?$usage}
        local topo_dir=${3:?$usage}
        local outdir=${4:?$usage}

        local cache_key_input=$(genesis profile-cache-key-input "$profile_json")
        local cache_key=$(genesis profile-cache-key "$profile_json")
        local cache_path=$cachedir/$cache_key

        if test -f "$cache_path"/cache.key
        then cache_hit=t; cache_hit_desc='hit'
        else cache_hit=;  cache_hit_desc='miss'; fi
        msg "genesis: preparing cache entry $cache_key:  $cache_hit_desc"

        if   test -z "$cache_hit"
        then regenesis_causes+=('cache-miss')
        elif test -v __REWRITE_GENESIS_CACHE
        then regenesis_causes+=('__REWRITE_GENESIS_CACHE-env-var-defined')
        fi

        if test -n "${regenesis_causes[*]}"
        then msg "genesis: generating due to ${regenesis_causes[*]}:  $cache_key @$cache_path"
             jqtest .genesis.single_shot "$profile_json" ||
                 fail "Incremental (non single-shot) genesis is not suppored."

             genesis actually-genesis "$profile_json" "$topo_dir" "$cache_path"

             cat <<<$cache_key_input > "$cache_path"/cache.key.input
             cat <<<$cache_key       > "$cache_path"/cache.key
        fi

        genesis finalise-cache-entry "$profile_json" "$cache_path"

        ## Install the cache entry:
        rm -f $outdir ## Must be a symlink for this to succeed
        ln -s "$(realpath "$cache_path")" "$outdir";;

    profile-cache-key-input )
        local usage="USAGE:  wb genesis profile-cache-key-input PROFILE-JSON"
        local profile_json=${1:?$usage}

        args=(--slurpfile profile $profile_json
              --sort-keys
              --null-input
             )
        ## Remove parts of profile that don't invalidate
        ## the cryptographic material in genesis.  Note the conservative approach.
        ##
        ## Note that the genesis cache entry itself must still be updated
        ## to match these parameters, hence the distinction between parameters:
        jq '
           $profile[0].genesis * $profile[0].composition

           ## Genesis-relevant -- must be updated in finalise-cache-entry:
           | del(.active_slots_coeff)
           | del(.epoch_length)
           | del(.max_block_size)
           | del(.max_tx_size)
           | del(.parameter_k)
           | del(.slot_duration)

           ## Genesis-irrelevant -- safe to ignore in finalise-cache-entry:
           | del(.byron)
           | del(.era)
           | del(.genesis_future_offset)
           | del(.locations)
           | del(.with_observer)
           ' ${args[*]};;

    profile-cache-key )
        local usage="USAGE:  wb genesis profile-cache-key PROFILE-JSON"
        local profile_json=${1:?$usage}

        args=(--arg params_hash $(genesis profile-cache-key-input $profile_json |
                                  sha1sum | cut -c-7)
              --raw-output
             )
        jq '[ "k\(.composition.n_pools)"
            , "d\(.composition.dense_pool_density)"
            , "\(.genesis.delegators / 1000)kD"
            , "\(.genesis.utxo / 1000)kU"
            , "\($params_hash)"
            ]
            | join("-")
            ' ${args[*]} $profile_json;;

    actually-genesis )
        local usage="USAGE:  wb actually-genesis PROFILE-JSON TOPO-DIR DIR"
        local profile_json=${1:?$usage}
        local topo_dir=${2:?$usage}
        local dir=${3:?$usage}

        rm -rf   "$dir"/{*-keys,pools,nodes,*.json,*.params}
        mkdir -p "$dir"

        cardano-cli genesis create --genesis-dir "$dir"/ \
            $(jq '.cli_args.createSpec | join(" ")' "$profile_json" --raw-output)

        ## Overlay the verbatim genesis part into the profile spec:
        local params=(
            --slurpfile profile "$profile_json"
            --raw-output
        )
        jq "${params[@]}" '
           . * $profile[0].genesis.verbatim
           '   "$dir"/genesis.spec.json |
        sponge "$dir"/genesis.spec.json

        params=(--genesis-dir "$dir"
                $(jq '.cli_args.createFinalBulk | join(" ")' "$profile_json" --raw-output)
               )
        cardano-cli genesis create-staked "${params[@]}"

        ## TODO: try to get rid of this step:
        Massage_the_key_file_layout_to_match_AWS "$profile_json" "$topo_dir" "$dir";;

    finalise-cache-entry )
        local usage="USAGE:  wb genesis finalise-cache-entry PROFILE-JSON DIR"
        local profile_json=${1:?$usage}
        local dir=${2:?$usage}

        ## Decide start time:
        local system_start_epoch=$(date '+%s' --date="now + $(jq .genesis.genesis_future_offset "$profile_json" --raw-output)")
        local system_start_human=$(date --date=@$system_start_epoch --utc +"%Y-%m-%dT%H:%M:%SZ")
        local start_time=$(date --date=@$system_start_epoch --utc --iso-8601=s |
                           cut -c-19)

        genesis_byron "$system_start_epoch" "$dir"

        jq ' $prof[0] as $p
           | . *
           { systemStart:                $start_time
           ##
           ## IMPORTANT: keep the rest in sync with the genesis-relevant bits
           ##            filtered out by "profile-cache-key-input".
           ##
           , activeSlotsCoeff:           $p.genesis.active_slots_coeff
           , epochLength:                $p.genesis.epoch_length
           , maxTxSize:                  $p.genesis.max_tx_size
           , securityParam:              $p.genesis.parameter_k
           , slotLength:                 $p.genesis.slot_duration
           , protocolParams:
             { "decentralisationParam":  $p.genesis.decentralisation_param
             , "maxBlockBodySize":       $p.genesis.max_block_size
             }
           }' --slurpfile prof       "$profile_json"  \
              --arg       start_time "${start_time}Z" \
               "$dir"/genesis.json |
        sponge "$dir"/genesis.json;;

    * ) usage_genesis;; esac
}

__KEY_ROOT=
Massage_the_key_file_layout_to_match_AWS() {
    local profile_json=${1:?$usage}
    local topo_dir=${2:?$usage}
    local dir=${3:?$usage}
    local ids

    set -euo pipefail

    local pool_density_map=$(topology density-map "$profile_json" "$topo_dir")
    msg "genesis: pool density map:  $pool_density_map"

    __KEY_ROOT=$dir

    ids=($(jq 'keys
              | join(" ")
              ' -cr <<<$pool_density_map))
    local bid=1 pid=1 did=1 ## (B)FT, (P)ool, (D)ense pool
    for id in ${ids[*]}
    do
        mkdir -p "$dir"/node-keys/cold

        #### cold keys (do not copy to production system)
        if   jqtest ".genesis.dense_pool_density > 1" $profile_json &&
             jqtest ".[\"$id\"]  > 1" <<<$pool_density_map
        then ## Dense/bulk pool
           msg "genesis:  bulk pool $did -> node-$id"
           cp -f $(key_genesis bulk      bulk $did) $(key_depl bulk   bulk $id)
           did=$((did + 1))
        elif jqtest ".[\"$id\"] != 0" <<<$pool_density_map
        then ## Singular pool
           msg "genesis:  pool $pid -> node-$id"
           cp -f $(key_genesis cold       sig $pid) $(key_depl cold    sig $id)
           cp -f $(key_genesis cold       ver $pid) $(key_depl cold    ver $id)
           cp -f $(key_genesis opcert    cert $pid) $(key_depl opcert none $id)
           cp -f $(key_genesis opcert   count $pid) $(key_depl cold  count $id)
           cp -f $(key_genesis KES        sig $pid) $(key_depl KES     sig $id)
           cp -f $(key_genesis KES        ver $pid) $(key_depl KES     ver $id)
           cp -f $(key_genesis VRF        sig $pid) $(key_depl VRF     sig $id)
           cp -f $(key_genesis VRF        ver $pid) $(key_depl VRF     ver $id)
           pid=$((pid + 1))
        else ## BFT node
           msg "genesis:  BFT $bid -> node-$id"
           cp -f $(key_genesis deleg      sig $bid) $(key_depl cold    sig $id)
           cp -f $(key_genesis deleg      ver $bid) $(key_depl cold    ver $id)
           cp -f $(key_genesis delegCert cert $bid) $(key_depl opcert none $id)
           cp -f $(key_genesis deleg    count $bid) $(key_depl cold  count $id)
           cp -f $(key_genesis delegKES   sig $bid) $(key_depl KES     sig $id)
           cp -f $(key_genesis delegKES   ver $bid) $(key_depl KES     ver $id)
           cp -f $(key_genesis delegVRF   sig $bid) $(key_depl VRF     sig $id)
           cp -f $(key_genesis delegVRF   ver $bid) $(key_depl VRF     ver $id)
           bid=$((bid + 1))
        fi
    done
}

key_depl() {
    local type=$1 kind=$2 id=$3
    case "$kind" in
            bulk )     suffix='.creds';;
            cert )     suffix='.cert';;
            count )    suffix='.counter';;
            none )     suffix=;;
            sig )      suffix='.skey';;
            ver )      suffix='.vkey';;
            * )        fail "key_depl: unknown key kind: '$kind'";; esac
    case "$type" in
            bulk )     stem=node-keys/bulk$id;;
            cold )     stem=node-keys/cold/operator$id;;
            opcert )   stem=node-keys/node$id.opcert;;
            KES )      stem=node-keys/node-kes$id;;
            VRF )      stem=node-keys/node-vrf$id;;
            * )        fail "key_depl: unknown key type: '$type'";; esac
    echo "$__KEY_ROOT"/$stem$suffix
}

key_genesis() {
    local type=$1 kind=$2 id=$3
    case "$kind" in
            bulk )     suffix='.creds';;
            cert )     suffix='.cert';;
            count )    suffix='.counter';;
            none )     suffix=;;
            sig )      suffix='.skey';;
            ver )      suffix='.vkey';;
            * )        fail "key_genesis: unknown key kind: '$kind'";; esac
    case "$type" in
            bulk )     stem=pools/bulk$id;;
            cold )     stem=pools/cold$id;;
            opcert )   stem=pools/opcert$id;;
            KES )      stem=pools/kes$id;;
            VRF )      stem=pools/vrf$id;;
            deleg )    stem=delegate-keys/delegate$id;;
            delegCert )stem=delegate-keys/opcert$id;;
            delegKES ) stem=delegate-keys/delegate$id.kes;;
            delegVRF ) stem=delegate-keys/delegate$id.vrf;;
            * )        fail "key_genesis: unknown key type: '$type'";; esac
    echo "$__KEY_ROOT"/$stem$suffix
}

genesis_byron()
{
    local system_start_epoch=$1
    local dir=$2

    jq '
      { heavyDelThd:       "300000"
      , maxBlockSize:      "641000"
      , maxHeaderSize:     "200000"
      , maxProposalSize:   "700"
      , maxTxSize:         "4096"
      , mpcThd:            "200000"
      , scriptVersion:     0
      , slotDuration:      "20000"
      , softforkRule:
        { initThd:         "900000"
        , minThd:          "600000"
        , thdDecrement:    "100000"
        }
      , txFeePolicy:
        { multiplier:      "439460"
        , summand:         "155381"
        }
      , unlockStakeEpoch:  "184467"
      , updateImplicit:    "10000"
      , updateProposalThd: "100000"
      , updateVoteThd:     "100000"
      }
    ' --null-input > "$dir"/byron-protocol-params.json
    cli_args=(
        ## Note that these parameters are irrelevant by now.
        --genesis-output-dir         "$dir"/byron
        --protocol-parameters-file   "$dir"/byron-protocol-params.json
        --start-time                 $system_start_epoch
        --k                          2160
        --protocol-magic             42
        --n-poor-addresses           1
        --n-delegate-addresses       1
        --total-balance              300000
        --delegate-share             0.9
        --avvm-entry-count           0
        --avvm-entry-balance         0
        )
    rm -rf "$dir"/byron
    cardano-cli byron genesis genesis "${cli_args[@]}"
}
