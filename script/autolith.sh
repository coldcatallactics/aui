# Find the Autolith that AUI is built against. Source this file instead of
# running it.
#
# Autolith can be installed in two ways, and AUI can be built against both:
#
#   from a release   The release directory contains bin/autolith, the source
#                    in libexec/autolith and a separate SBCL in runtime.
#   from source      The checkout contains autolith.asd and bin/autolith. The
#                    SBCL chosen by the bootstrap is recorded in the data
#                    directory.
#
# Set AUI_AUTOLITH_ROOT or AUTOLITH_SOURCE_ROOT to the release directory or the
# checkout. AUI_AUTOLITH_ROOT is used first. If neither is set, the Autolith
# behind the autolith command on the path is used, or else the Autolith behind
# ~/.local/bin/autolith.
#
# This file sets these variables:
#   autolith_source   the directory that contains autolith.asd, or empty
#   autolith_release  the release directory, or empty for a source checkout
#   autolith_sbcl     the SBCL that Autolith runs on, or empty
#   autolith_identity one line with the source directory and the version of
#                     this Autolith

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
  # The Autolith installer links the command into ~/.local/bin. That directory
  # is not always on the path of a login.
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
