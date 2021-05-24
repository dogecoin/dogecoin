#!/usr/bin/env bash
# shellcheck disable=SC1090,SC2154,SC2155,SC2034,SC2039,SC2240,SC2124,SC2209
##
## NOTE: this must be imported strictly _after_ 'common.sh'
##

setup_genesis_for_config() {
        dprint "setup_genesis_for_config: $*"
        local genesis_config="$1"

        configuration_root="${configuration}/defaults/${genesis_config}"
        genesis_root="${configuration_root}/genesis"
        genesis_file="${genesis_root}/genesis.json"

        if test -r "${genesis_file}" -a -z "${force_genesis}";
        then :
        else genesis "${genesis_root}"
        fi

        genesis_hash=$(cat "${genesis_root}"/GENHASH)

        vprint_top "using genesis ${genesis_root} with hash ${genesis_hash}"
}

genesis() {
        dprint "genesis: $*"
        local outdir="$1" start_future_offset="${2:-1 minute}"

        local parameter_k=2160
        local protocol_magic=459045235
        local n_poors=128
        local n_delegates=7
        local total_balance=8000000000000000
        local delegate_share=0.9
        local avvm_entries=128
        local avvm_entry_balance=10000000000000
        local not_so_secret=2718281828

        local OS=$(uname -s) DATE=
        case $OS in
                Darwin )       DATE=gdate;;
                * )            DATE=date;; esac

        local start_time="$(${DATE} -d "now + ${start_future_offset}" +%s)"
        local protocol_params="${scripts}/protocol-params.json"

        local tmpdir="$(mktemp).d"
        ARGS=(
                --genesis-output-dir           "${tmpdir}"
                --start-time                   "${start_time}"
                --protocol-parameters-file     "${protocol_params}"
                --k                            "${parameter_k}"
                --protocol-magic               "${protocol_magic}"
                --n-poor-addresses             "${n_poors}"
                --n-delegate-addresses         "${n_delegates}"
                --total-balance                "${total_balance}"
                --avvm-entry-count             "${avvm_entries}"
                --avvm-entry-balance           "${avvm_entry_balance}"
                --delegate-share               "${delegate_share}"
                --real-pbft
                --secret-seed                  "${not_so_secret}"
        )
        run_quiet cardano-cli cardano-cli byron genesis genesis "${ARGS[@]}"

        # move new genesis to configuration
        local GENHASH=$(run_quiet cardano-cli cardano-cli print-genesis-hash --genesis-json "${tmpdir}/genesis.json" | tail -1)
        mkdir -p "${outdir}"
        cp -af "${tmpdir}"/genesis.json "${outdir}"/
        cp -af "${tmpdir}"/delegate-keys.*.key "${outdir}"/
        cp -af "${tmpdir}"/delegation-cert.*.json "${outdir}"/

        echo "$GENHASH" > "${outdir}"/GENHASH
        vprint "genesis created with hash = ${GENHASH}"
        vprint "  in directory ${outdir}"
}

get_default_key_address() {
        dprint "get_default_key_address: $*"
        local proto_magic=$1 key=$2

        local ARGS=(
                signing-key-address
                --real-pbft
                --testnet-magic "${proto_magic}"
                --secret "${key}"
        )
        run_quiet cardano-cli cardano-cli "${ARGS[@]}" | head -n1 | xargs echo -n
}

issue_genesis_utxo_expenditure() {
        dprint "issue_genesis_utxo_expenditure: $*"
        local config="$1" tx="$2"
        local from_key="$3" from_addr="$4" lovelace="$5"
        local to_addr="$6"

        local ARGS=(
                issue-genesis-utxo-expenditure
                --config              "${config}"
                --tx                  "${tx}"
                --wallet-key          "${from_key}"
                --rich-addr-from      "\"${from_addr}\""
                --txout               "(\"${to_addr}\",${lovelace})"
        )
        run_quiet cardano-cli cardano-cli "${ARGS[@]}"
}

issue_utxo_expenditure() {
        dprint "issue_utxo_expenditure: $*"
        local config="$1" tx="$2"
        local from_key="$3" txin="$4" txinoutputindex="$5" lovelace="$6"
        local to_addr="$7"

        local ARGS=(
                issue-utxo-expenditure
                --config              "${config}"
                --tx                  "${tx}"
                --wallet-key          "${from_key}"
                --txin                "(\"${txin}\",${txinoutputindex})"
                --txout               "(\"${to_addr}\",${lovelace})"
        )
        run_quiet cardano-cli cardano-cli "${ARGS[@]}"
}
