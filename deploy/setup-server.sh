#!/usr/bin/env bash
# Setup-Skript für astro.wetterheidi.de
# Ausführen als root auf dem Hetzner-Server
set -euo pipefail

DOMAIN="astro.wetterheidi.de"
APP_DIR="/apps/astro"
NGINX_CONF="/etc/nginx/sites-available/${DOMAIN}"
NGINX_ENABLED="/etc/nginx/sites-enabled/${DOMAIN}"

echo "[1/5] App-Verzeichnis anlegen ..."
mkdir -p "$APP_DIR"

echo "[2/5] Dateien kopieren ..."
# Skript liegt im deploy/-Ordner, Projektroot ist eine Ebene höher
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"
cp "$REPO_ROOT/Astro_v2.html" "$APP_DIR/"

# Bilder-Ordner mitkopieren, falls vorhanden
if [ -d "$REPO_ROOT/images" ]; then
    cp -r "$REPO_ROOT/images" "$APP_DIR/"
fi

echo "[3/5] Berechtigungen setzen ..."
chown -R www-data:www-data "$APP_DIR"
chmod -R 755 "$APP_DIR"

echo "[4/5] nginx-Config einspielen ..."
cp "$SCRIPT_DIR/nginx-astro.conf" "$NGINX_CONF"
ln -sf "$NGINX_CONF" "$NGINX_ENABLED"

echo "[5/5] SSL via certbot einrichten & nginx neu laden ..."
certbot --nginx -d "$DOMAIN"
nginx -t && systemctl reload nginx

echo ""
echo "Done! https://${DOMAIN} sollte jetzt erreichbar sein."
echo "Nutzer anlegen (falls nötig): htpasswd /etc/nginx/.htpasswd-wetterheidi <nutzername>"
