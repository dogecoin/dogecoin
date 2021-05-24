usage_profile() {
     usage "profile" "Cluster profile operations" <<EOF
    list                  List profile names (json)
    all-profiles | all    All profile contents (json)
    compose NAME..        Create a profile composed from named profiles
    get NAME              Get contents of either named profile, or profile JSON desc
    describe NAME         Print a human description of a profile
    node-specs PROFILE-JSON ENV-JSON
                          Print node specs JSON for the given profile and environment
EOF
}

global_profile_eras=(
    shelley
    allegra
    mary
)

profile() {
local op=${1:-list}; test $# -gt 0 && shift

case "${op}" in
    list | names )
        profile generate-all | jq 'keys'
        ;;

    all-profiles | generate-all | all )
        with_era_profiles '
          map (profiles(.; null; null; []))
          | add
        ';;

    all-profile-names | names | all-names )
        with_era_profiles '
          map (profile_names(.; null; null; []))
          | add
        ';;

    has-profile )
        local usage="USAGE: wb profile has-profile NAME"
        local name=${1:?$usage}

        with_era_profiles '
          map (has_profile(.; null; null; []; $name))
          | any
        ' --exit-status --arg name "$name" >/dev/null
        ;;

    compose )
        local profile_names="$@"

        profile generate-all |
        jq --argjson profile_names "$(to_jsonlist ${profile_names[*]})" '
          . as $profiles
          | $profile_names | debug
          | map($profiles[.])
          | add
          ';;

    get )
        local usage="USAGE: wb profile get NAME"
        local name=${1:?$usage}

        if test -f  "$name"
        then jq '.' "$name"
        else profile generate-all |
             jq '.["'$name'"]'
        fi
        ;;

    describe )
        local usage="USAGE: wb profile describe NAME"
        local name=${1:?$usage}

        profile get $name |
        (cd "$global_basedir/profiles";

         echo -n "workbench:  "
         jq '
          include "derived";
          profile_pretty_describe(.)
          ' --raw-output);;

    node-specs )
        local usage="USAGE: wb profile node-specs PROFILE-JSON ENV-JSON"
        local profile_json=${1:?$usage}
        local env_json=${2:?$usage}

        args=(
            "$profile_json"
            --slurpfile env "$env_json"
        )
        jq '. as $prof
           | $prof.composition.n_bft_hosts  as $n_bfts
           | $prof.composition.n_pool_hosts as $n_pools
           | ([range(0;
                     $n_bfts)]
              | map({ i: .
                    , kind: "bft"
                    }))
              as $bfts
           | ([range($n_bfts;
                     $n_bfts + $n_pools)]
              | map({ i: .
                    , kind: "pool"
                    }))
              as $pools
           | ([range($n_bfts + $n_pools;
                     $n_bfts + $n_pools +
                     if $prof.composition.with_observer then 1 else 0 end)]
              | map({ i: .
                    , kind: "observer"
                    }))
              as $observers
           | ($bfts + $pools + $observers
              | map(. +
                    { name:       "node-\(.["i"])"
                    , isProducer: ([.kind == "bft", .kind == "pool"] | any)
                    , port:
                      (if $env[0].staggerPorts
                       then $env[0].basePort + .i
                       else $env[0].basePort
                       end)
                    }))
           | map({ key: .name, value: .})
           | from_entries
           ' "${args[@]}";;

    * ) usage_profile;; esac
}

with_era_profiles() {
    local usage="USAGE: wb profile with-profiles JQEXP"
    local jqexp=${1:?$usage}; shift

    jq  -L "$global_basedir/profiles" \
        --argjson eras "$(to_jsonlist ${global_profile_eras[*]})" \
        --null-input '
       include "profiles";

       $eras | '"$jqexp" "$@"
}
