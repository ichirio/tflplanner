#!/bin/sh
# The tflplanner launcher for macOS and Linux, run by its shortcuts
# (tflplanner::add_shortcut() writes this file).
#
# It is made for one R x.y, which launcher.conf (beside this file) records:
#   r_home=<the R it was made in>   r_xy=<its x.y, "4.6">
# It looks for Rscript at every start, in this order, and never falls back
# to another x.y -- every R it picks has its x.y checked (asked of the
# Rscript: --version), the remembered one too, since R.framework/Resources
# and /usr/lib/R are whichever R is current (the rule is .pick_r() in
# tflplanner's R/launcher.R; the tests run one table of cases through it,
# this script and tflplanner.vbs):
#   1. the remembered R (r_home), when its bin/Rscript is there and is this x.y;
#   2. the newest R of this x.y in the usual install places and beside the
#      remembered R (its parent folder); the first found among equal versions
#      (on macOS the build of this machine's architecture comes first);
#   3. the first Rscript on PATH (every entry, in order) of this x.y;
#   4. else it says that R x.y is not found, and stops.
# Then it runs launch.R.
# Arguments (--update, --port=N) are passed on to launch.R.
# --which: print the Rscript it would use and start nothing (for the tests;
# TFLPLANNER_R_SEARCH, glob patterns separated by ":", replaces the usual
# places).
HERE="$(cd "$(dirname "$0")" && pwd)"
CONF="$HERE/launcher.conf"
conf() { [ -f "$CONF" ] && sed -n "s/^$1=//p" "$CONF" | head -n 1 | tr -d '\r'; }
XY="$(conf r_xy)"
RHOME="$(conf r_home)"
WHICH=""
for a in "$@"; do [ "$a" = "--which" ] && WHICH=1; done

# the full version of an Rscript ("4.6.1"), from what it says of itself
full_of() {
  "$1" --version 2>&1 | sed -n 's/.*version \([0-9][0-9]*\.[0-9][0-9]*[0-9.]*\).*/\1/p' | head -n 1
}
# its x.y ("4.6")
xy_of() {
  full_of "$1" | sed -n 's/^\([0-9][0-9]*\.[0-9][0-9]*\).*/\1/p'
}
# is $1 newer than $2?  numbers, part by part ("4.10.0" is newer than "4.9.3")
newer() {
  awk -v a="$1" -v b="$2" 'BEGIN {
    n = split(a, x, "."); m = split(b, y, ".")
    if (m > n) n = m
    for (i = 1; i <= n; i++) {
      p = x[i] + 0; q = y[i] + 0
      if (p > q) exit 0
      if (p < q) exit 1
    }
    exit 1 }'
}

# the Rscripts of the usual places, one per line (those that are there)
candidates() {
  ARCH="$(uname -m 2>/dev/null)"
  FW=/Library/Frameworks/R.framework/Versions
  PARENT="$(dirname "$RHOME")"
  if [ -n "$TFLPLANNER_R_SEARCH" ]; then
    pats="$TFLPLANNER_R_SEARCH"
  else
    pats="$FW/$XY-$ARCH/Resources/bin/Rscript:$FW/$XY/Resources/bin/Rscript"
    pats="$pats:$FW/$XY*/Resources/bin/Rscript:$FW/Current/Resources/bin/Rscript"
    pats="$pats:/opt/R/$XY.*/bin/Rscript"
    pats="$pats:/usr/lib/R/bin/Rscript:/usr/lib64/R/bin/Rscript"
    pats="$pats:/usr/local/lib/R/bin/Rscript:/usr/local/lib64/R/bin/Rscript"
    pats="$pats:/opt/homebrew/bin/Rscript:/usr/local/bin/Rscript:/usr/bin/Rscript"
  fi
  # beside the remembered R: its parent folder's R-x.y.* and x.y.*
  [ -n "$RHOME" ] && pats="$pats:$PARENT/R-$XY.*/bin/Rscript:$PARENT/$XY.*/bin/Rscript"
  oldifs="$IFS"
  IFS=:
  for pat in $pats; do
    IFS="$oldifs"
    for c in $pat; do
      [ -x "$c" ] && printf '%s\n' "$c"
    done
    IFS=:
  done
  IFS="$oldifs"
}

R=""
if [ -n "$XY" ]; then
  # 1. the remembered R, of this x.y
  if [ -n "$RHOME" ] && [ -x "$RHOME/bin/Rscript" ] &&
     [ "$(xy_of "$RHOME/bin/Rscript")" = "$XY" ]; then
    R="$RHOME/bin/Rscript"
  fi
  # 2. the newest R of this x.y in the usual places
  if [ -z "$R" ]; then
    BESTV=""
    cands="$(candidates)"
    while IFS= read -r c; do
      [ -n "$c" ] || continue
      [ "$(xy_of "$c")" = "$XY" ] || continue
      v="$(full_of "$c")"
      if [ -z "$BESTV" ] || newer "$v" "$BESTV"; then
        R="$c"
        BESTV="$v"
      fi
    done <<EOF
$cands
EOF
  fi
  # 3. the first Rscript on PATH (every entry, in order) of this x.y
  if [ -z "$R" ]; then
    entries="$(printf '%s' "$PATH" | tr ':' '\n')"
    while IFS= read -r d; do
      [ -n "$d" ] && [ -x "$d/Rscript" ] || continue
      if [ "$(xy_of "$d/Rscript")" = "$XY" ]; then R="$d/Rscript"; break; fi
    done <<EOF
$entries
EOF
  fi
fi
if [ -n "$WHICH" ]; then
  [ -n "$R" ] && { printf '%s\n' "$R"; exit 0; }
  exit 1
fi
if [ -z "$R" ]; then
  if command -v osascript >/dev/null 2>&1; then
    osascript -e 'display alert "tflplanner" message "{{NO_R}}"'
  elif command -v notify-send >/dev/null 2>&1; then
    notify-send "tflplanner" "{{NO_R}}"
  fi
  echo "tflplanner: {{NO_R}}" >>"$HERE/launcher.log"
  exit 1
fi
exec "$R" "$HERE/launch.R" "$@" >>"$HERE/launcher.log" 2>&1
