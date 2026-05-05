#!/usr/bin/env bash
# Update-Skript: git pull + HTML-Dateien einspielen
# Ausführen als root auf dem Hetzner-Server
set -euo pipefail

APP_DIR="/apps/astro"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"

echo "[1/3] Git pull ..."
git -C "$REPO_ROOT" pull

echo "[2/3] Deploying Astro_v2.html ..."
cp "$REPO_ROOT/Astro_v2.html" "$APP_DIR/"

if [ -d "$REPO_ROOT/images" ]; then
    cp -r "$REPO_ROOT/images" "$APP_DIR/"
fi

echo "[3/3] Berechtigungen setzen ..."
chown -R www-data:www-data "$APP_DIR"
echo "Done."
