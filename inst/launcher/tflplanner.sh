#!/bin/sh
# The tflplanner launcher for macOS and Linux, run by its shortcuts
# (tflplanner::add_shortcut() writes this file).
#
# It is made for one R x.y, which launcher.conf (beside this file) records:
#   r_home=<the R it was made in>   r_xy=<its x.y, "4.6">
# It looks for Rscript at every start, in this order, and never falls back
# to another x.y (the rule is .pick_r() in tflplanner's R/launcher.R):
#   1. the remembered R, when its bin/Rscript is there;
#   2. the newest R of the same x.y in the usual install places;
#   3. Rscript on PATH, only when its x.y matches;
#   4. else it says that R x.y is not found, and stops.
# Then it runs launch.R.
# Arguments (--update, --port=N) are passed on to launch.R.
HERE="$(cd "$(dirname "$0")" && pwd)"
CONF="$HERE/launcher.conf"
conf() { [ -f "$CONF" ] && sed -n "s/^$1=//p" "$CONF" | head -n 1 | tr -d '\r'; }
XY="$(conf r_xy)"
RHOME="$(conf r_home)"

# the x.y of an Rscript ("4.6"), from what it says of its version
xy_of() {
  "$1" --version 2>&1 | sed -n 's/.*version \([0-9][0-9]*\)\.\([0-9][0-9]*\).*/\1.\2/p' | head -n 1
}
# the full version ("4.6.1"), to tell the newest apart
full_of() {
  "$1" --version 2>&1 | sed -n 's/.*version \([0-9][0-9.]*[0-9]\).*/\1/p' | head -n 1
}

R=""
if [ -n "$XY" ]; then
  # 1. the remembered R
  if [ -n "$RHOME" ] && [ -x "$RHOME/bin/Rscript" ]; then
    R="$RHOME/bin/Rscript"
  fi
  # 2. the newest R of this x.y in the usual places
  if [ -z "$R" ]; then
    BESTV=""
    for c in \
      /Library/Frameworks/R.framework/Versions/"$XY"*/Resources/bin/Rscript \
      /Library/Frameworks/R.framework/Resources/bin/Rscript \
      /opt/R/"$XY".*/bin/Rscript \
      /usr/lib/R/bin/Rscript /usr/lib64/R/bin/Rscript \
      /usr/local/lib/R/bin/Rscript /usr/local/lib64/R/bin/Rscript \
      /opt/homebrew/bin/Rscript /usr/local/bin/Rscript /usr/bin/Rscript; do
      [ -x "$c" ] || continue
      [ "$(xy_of "$c")" = "$XY" ] || continue
      v="$(full_of "$c")"
      if [ -z "$BESTV" ] || \
         [ "$(printf '%s\n%s\n' "$BESTV" "$v" | sort -V | tail -n 1)" = "$v" ]; then
        R="$c"
        BESTV="$v"
      fi
    done
  fi
  # 3. Rscript on PATH, only when its x.y matches
  if [ -z "$R" ]; then
    c="$(command -v Rscript 2>/dev/null)"
    if [ -n "$c" ] && [ "$(xy_of "$c")" = "$XY" ]; then R="$c"; fi
  fi
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
