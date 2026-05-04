#!/usr/bin/env bash
# Schnelles Update-Skript: nur HTML-Dateien neu einspielen
# Ausführen als root auf dem Hetzner-Server
set -euo pipefail

APP_DIR="/apps/astro"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"

echo "Deploying Astro_v2.html ..."
cp "$REPO_ROOT/Astro_v2.html" "$APP_DIR/"

if [ -d "$REPO_ROOT/images" ]; then
    cp -r "$REPO_ROOT/images" "$APP_DIR/"
fi

chown -R www-data:www-data "$APP_DIR"
echo "Done."
