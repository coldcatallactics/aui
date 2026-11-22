# Source this file to find the Autolith source, release directory and SBCL.
# Compare autolith_identity (source path and version) to the saved image stamp.

aui_follow_links() {
  local path=$1 directory
  while [[ -L "$path" ]]; do
    directory=$(CDPATH= cd -P "$(dirname "$path")" && pwd)
    path=$(readlink "$path")
    [[ $path == /* ]] || path=$directory/$path
  done
  printf '%s\n' "$path"
}

autolith_source=
autolith_release=
autolith_sbcl=${AUTOLITH_SBCL:-}
autolith_identity=

autolith_home=${AUI_AUTOLITH_ROOT:-${AUTOLITH_SOURCE_ROOT:-}}
if [[ -z "$autolith_home" ]]; then
  autolith_command=$(command -v autolith 2>/dev/null) || autolith_command=$HOME/.local/bin/autolith
  if [[ -e "$autolith_command" ]]; then
    autolith_command=$(aui_follow_links "$autolith_command")
    autolith_home=$(dirname "$(dirname "$autolith_command")")
  fi
fi
if [[ -n "$autolith_home" && -d "$autolith_home" ]]; then
  autolith_home=$(CDPATH= cd -P "$autolith_home" && pwd)
  if [[ -f "$autolith_home/autolith.asd" ]]; then
    autolith_source=$autolith_home
    # The source directory inside a release, given directly.
    if [[ -f "$autolith_home/../../RELEASE" && -x "$autolith_home/../../bin/autolith" ]]; then
      autolith_release=$(CDPATH= cd -P "$autolith_home/../.." && pwd)
    fi
  elif [[ -f "$autolith_home/libexec/autolith/autolith.asd" ]]; then
    autolith_release=$autolith_home
    autolith_source=$autolith_home/libexec/autolith
  fi
fi

if [[ -z "$autolith_sbcl" && -n "$autolith_release" && -x "$autolith_release/runtime/bin/sbcl" ]]; then
  autolith_sbcl=$autolith_release/runtime/bin/sbcl
fi
if [[ -z "$autolith_sbcl" ]]; then
  case ${XDG_DATA_HOME:-} in
    /*) autolith_data_home=$XDG_DATA_HOME ;;
    *) autolith_data_home=$HOME/.local/share ;;
  esac
  if [[ -r "$autolith_data_home/autolith/runtimes/command" ]]; then
    IFS= read -r autolith_sbcl < "$autolith_data_home/autolith/runtimes/command" || autolith_sbcl=
  fi
fi
if [[ -n "$autolith_sbcl" && ! -x "$autolith_sbcl" ]]; then
  autolith_sbcl=
fi

if [[ -n "$autolith_source" ]]; then
  autolith_version=
  if [[ -r "$autolith_source/VERSION" ]]; then
    IFS= read -r autolith_version < "$autolith_source/VERSION" || true
  fi
  autolith_identity="$autolith_source $autolith_version"
fi
