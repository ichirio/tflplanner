#!/bin/sh
# The tflplanner launcher for macOS and Linux, run by its shortcuts
# (tflplanner::add_shortcut() writes this file).  Finds Rscript at every
# start -- R.framework's current R on macOS, else PATH, else the usual
# places -- so an R update does not break the shortcut, and runs launch.R.
# Arguments (--update, --port=N) are passed on to launch.R.
HERE="$(cd "$(dirname "$0")" && pwd)"
R=""
for c in /Library/Frameworks/R.framework/Resources/bin/Rscript; do
  [ -x "$c" ] && R="$c"
done
[ -n "$R" ] || R="$(command -v Rscript 2>/dev/null)"
if [ -z "$R" ]; then
  for c in /opt/homebrew/bin/Rscript /usr/local/bin/Rscript /usr/bin/Rscript; do
    if [ -x "$c" ]; then R="$c"; break; fi
  done
fi
if [ -z "$R" ]; then
  if command -v osascript >/dev/null 2>&1; then
    osascript -e 'display alert "tflplanner" message "{{NO_R}}"'
  elif command -v notify-send >/dev/null 2>&1; then
    notify-send "tflplanner" "{{NO_R}}"
  fi
  exit 1
fi
exec "$R" "$HERE/launch.R" "$@" >>"$HERE/launcher.log" 2>&1
