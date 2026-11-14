# Find Lightfast and build its native bridge. Sourced, not run, after the
# script has set root to AUI's directory.
#
# Sets:
#   lightfast               Lightfast's checkout: AUI_LIGHTFAST_ROOT, or else
#                           the one beside AUI
# Defines:
#   aui_require_lightfast   say where Lightfast should be when it is not there
#   aui_build_bridge        build the bridge there when it is stale

lightfast=${AUI_LIGHTFAST_ROOT:-$root/../lightfast}

aui_require_lightfast() {
  if [[ ! -f "$lightfast/lightfast.asd" ]]; then
    echo "Lightfast is not at $lightfast. Clone https://github.com/lambda-symbolics/lightfast beside AUI, or set AUI_LIGHTFAST_ROOT." >&2
    return 64
  fi
}

aui_build_bridge() {
  local make
  aui_require_lightfast || return
  # Lightfast's Makefile is GNU make's, which the BSDs install as gmake.
  make=$(command -v gmake || command -v make) || {
    echo "AUI needs GNU make to build Lightfast's native bridge." >&2
    return 64
  }
  "$make" -s -C "$lightfast" native
}
