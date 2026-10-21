#!/usr/bin/env bash
# Fetch a URL to a file, retrying transient failures with exponential backoff.
# This exists because curl's own --retry only retries a curl-defined set of
# "transient" errors (timeouts, HTTP 408/429/5xx) and silently does NOT retry
# things like a mid-handshake connection reset (curl exit 35) -- exactly the
# kind of CI network blip this is meant to absorb. --retry-all-errors would
# fix that, but it needs curl >= 7.71 and not every CI image here has it, so
# we do the retry loop ourselves instead of depending on a curl version floor.
#
# Usage: download-retry.sh <connect-timeout-secs> <max-attempts> <outfile> <url>
set -o pipefail

connect_timeout="$1"
max_attempts="$2"
outfile="$3"
url="$4"

attempt=1
delay=1
while true; do
    curl --location --fail --connect-timeout "$connect_timeout" -o "$outfile" "$url"
    status=$?
    if [ "$status" -eq 0 ]; then
        exit 0
    fi
    if [ "$attempt" -ge "$max_attempts" ]; then
        exit "$status"
    fi
    echo "curl failed (exit $status) on attempt $attempt/$max_attempts, retrying in ${delay}s..." >&2
    sleep "$delay"
    attempt=$((attempt + 1))
    delay=$((delay * 2))
done
