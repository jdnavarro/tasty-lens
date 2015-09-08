#!/bin/sh
set -e

type grep-aptavail >/dev/null 2>&1 || {
    echo >&2 "grep-aptavail (dctrl-tools) needs to be installed";
    exit 1;
}
. /etc/os-release
echo "-- $PRETTY_NAME"
echo "-- Created on $(date)"
grep-aptavail -F Ghc-Package . -s Ghc-Package -n \
    | sort \
    | perl -ne 'BEGIN {print "constraints:\n"} /^(.*)-([0-9\.]*)-(.*)$/; print "  $1 ==$2,\n"' \
    | sed '$s/,$//' \
