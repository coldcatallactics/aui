# Find Lightfast and build the native bridge of Lightfast. Source this file
# instead of running it. The variable root must contain the directory of AUI.
#
# This file sets this variable:
#   lightfast               the Lightfast checkout: AUI_LIGHTFAST_ROOT, or else
#                           the checkout beside AUI
# This file defines these functions:
#   aui_require_lightfast   print the expected place of Lightfast if it is missing
#   aui_build_bridge        build the native bridge if it is stale

lightfast=${AUI_LIGHTFAST_ROOT:-$root/../lightfast}

aui_require_lightfast() {
  if [[ ! -f "$lightfast/lightfast.asd" ]]; then
    echo "Lightfast was not found at $lightfast. Clone https://github.com/lambda-symbolics/lightfast beside AUI, or set AUI_LIGHTFAST_ROOT." >&2
    return 64
  fi
}

aui_build_bridge() {
  local make
  aui_require_lightfast || return
  # The Makefile of Lightfast needs GNU make. On the BSDs, GNU make is installed
  # as gmake.
  make=$(command -v gmake || command -v make) || {
    echo "AUI needs GNU make to build the native bridge of Lightfast." >&2
    return 64
  }
  "$make" -s -C "$lightfast" native
}
