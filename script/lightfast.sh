# Locate Lightfast and provide helpers for building its native bridge.
# The caller sets root to the AUI directory.

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
    # BSD systems commonly provide GNU make as gmake.
  make=$(command -v gmake || command -v make) || {
    echo "AUI needs GNU make to build the native bridge of Lightfast." >&2
    return 64
  }
  "$make" -s -C "$lightfast" native
}
