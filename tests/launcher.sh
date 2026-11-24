#!/usr/bin/env bash
# Switch Autolith installations without changing any AUI source or timestamps.
set -euo pipefail
root=$(CDPATH= cd -P "$(dirname "$0")/.." && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
mkdir -p "$tmp"/{bin,build,script,src,old,new,lightfast/src,lightfast/native}
cp "$root/bin/aui" "$tmp/bin/"
cp "$root/script/autolith.sh" "$tmp/script/"
touch "$tmp"/{aui,old/autolith,new/autolith}.asd
printf '0.1\n' | tee "$tmp/old/VERSION" > "$tmp/new/VERSION"
printf '%s 0.1\n' "$tmp/old" > "$tmp/build/autolith"
cat > "$tmp/build/aui" <<'IMAGE'
#!/usr/bin/env bash
cat "$(dirname "$0")/autolith"
IMAGE
cat > "$tmp/script/build" <<'BUILD'
#!/usr/bin/env bash
source "$(dirname "$0")/autolith.sh"
printf '%s\n' "$autolith_identity" > "$(dirname "$0")/../build/autolith"
BUILD
chmod +x "$tmp/build/aui" "$tmp/script/build"
actual=$(AUI_AUTOLITH_ROOT="$tmp/new" AUI_LIGHTFAST_ROOT="$tmp/lightfast" "$tmp/bin/aui")
expected="$(cd -P "$tmp/new" && pwd) 0.1"
[[ $actual == "$expected" ]] || { printf 'Expected %s, launched %s\n' "$expected" "$actual" >&2; exit 1; }
# A new commit may change the sidebar protocol without changing VERSION.
git -C "$tmp/new" init -q
git -C "$tmp/new" -c user.name=test -c user.email=test@example.org commit -qm initial --allow-empty
expected+=" $(git -C "$tmp/new" rev-parse HEAD)"
actual=$(AUI_AUTOLITH_ROOT="$tmp/new" AUI_LIGHTFAST_ROOT="$tmp/lightfast" "$tmp/bin/aui")
[[ $actual == "$expected" ]]
printf '#!/bin/sh\nexit 1\n' > "$tmp/script/build"
if AUI_AUTOLITH_ROOT="$tmp/old" AUI_LIGHTFAST_ROOT="$tmp/lightfast" "$tmp/bin/aui"; then
  echo 'Started an image for another installation after a failed build' >&2
  exit 1
fi
