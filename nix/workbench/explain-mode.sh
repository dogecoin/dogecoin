explain-mode() {
    local message=""
    local execmode=$(cut -d+ -f1 <<<$1 | cut -d- -f1)
    local wbmode=$(cut   -d+ -f2 <<<$1 | cut -d- -f1)

    message+="executables are "
    case "$execmode" in
        nix )      message+="pre-supplied by Nix (at shell entry)";;
        cabal )    message+="built by Cabal (on demand)";;
    esac

    message+=", wb script is "
    case "$wbmode" in
        nix )      message+="pre-supplied by Nix (at shell entry)";;
        checkout ) message+="sourced from the local checkout";;
    esac
    message+="."

    msg "$message"
}
