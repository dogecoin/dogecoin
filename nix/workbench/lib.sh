to_jsonlist() {
    for x in "$@"
    do echo "\"$x\""
    done | jq --slurp '.'
}

jq_tolist() {
    local exp=$1; shift
    jq "$exp | join (\" \")" --raw-output "$@"
}

jq_fmutate() {
    local f=$1; shift
    test -f "$f" || { echo '{}' > "$f"; }
    jq "$@" "$f" | sponge "$f"
}

jq_check_json() {
    jq '.' "$1" >/dev/null
}

__usage() {
    local op=$1 desc=$2
    cat >&2 <<EOF
USAGE:  $(basename "$0") OPTIONS.. $op SUBOP SUBOP-ARGS..

  $desc:

$(cat)

EOF
}

usage() {
    __usage "$@"
    exit 1
}

msg() {
    echo "workbench:  $*" >&2
}

fail() {
    msg "$*"
    exit 1
}

fatal() {
    fail "FATAL: $*"
}

jqtest() {
    jq --exit-status "$@" > /dev/null
}
