#!/usr/bin/env bash
# Idempotent Cloud Agent setup for the CFB Playoff Prediction R project.
# Installs R (Ubuntu 24.04 "noble"), the CRAN packages the model needs as
# fast r2u binary .debs, and the Quarto CLI used to render the model card.
set -euo pipefail

export DEBIAN_FRONTEND=noninteractive

KEYRING_DIR=/etc/apt/keyrings
CRAN_KEY="$KEYRING_DIR/cran.gpg"
R2U_KEY="$KEYRING_DIR/r2u.gpg"

echo "==> Ensuring apt prerequisites"
sudo apt-get update -qq
sudo apt-get install -y --no-install-recommends \
  wget ca-certificates gnupg lsb-release

echo "==> Configuring CRAN + r2u apt repositories (Ubuntu noble)"
sudo install -d -m 0755 "$KEYRING_DIR"
if [ ! -s "$CRAN_KEY" ]; then
  wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc \
    | sudo gpg --dearmor -o "$CRAN_KEY"
fi
if [ ! -s "$R2U_KEY" ]; then
  wget -qO- https://eddelbuettel.github.io/r2u/assets/dirk_eddelbuettel_key.asc \
    | sudo gpg --dearmor -o "$R2U_KEY"
fi
echo "deb [signed-by=$CRAN_KEY] https://cloud.r-project.org/bin/linux/ubuntu noble-cran40/" \
  | sudo tee /etc/apt/sources.list.d/cran.list >/dev/null
echo "deb [signed-by=$R2U_KEY] https://r2u.stat.illinois.edu/ubuntu noble main" \
  | sudo tee /etc/apt/sources.list.d/cranapt.list >/dev/null
sudo apt-get update -qq

echo "==> Installing R and CRAN package dependencies (r2u binaries)"
sudo apt-get install -y --no-install-recommends \
  r-base-core \
  r-cran-tidyverse \
  r-cran-caret \
  r-cran-randomforest \
  r-cran-xgboost \
  r-cran-duckdb \
  r-cran-dbi \
  r-cran-httr \
  r-cran-jsonlite \
  r-cran-readxl \
  r-cran-rvest \
  r-cran-xml2 \
  r-cran-withr \
  r-cran-zoo \
  r-cran-reshape2 \
  r-cran-testthat \
  r-cran-data.table \
  r-cran-commonmark \
  r-cran-digest \
  r-cran-cfbfastr

echo "==> Ensuring Quarto CLI is installed (renders the static model dashboard)"
if ! command -v quarto >/dev/null 2>&1; then
  QVER="$(curl -sSL -m 20 https://quarto.org/docs/download/_download.json 2>/dev/null \
    | grep -oE '"version": *"[0-9.]+"' | head -1 | grep -oE '[0-9.]+' || true)"
  QVER="${QVER:-1.6.42}"
  tmpdeb="$(mktemp --suffix=.deb)"
  curl -sSL -m 180 -o "$tmpdeb" \
    "https://github.com/quarto-dev/quarto-cli/releases/download/v${QVER}/quarto-${QVER}-linux-amd64.deb"
  sudo dpkg -i "$tmpdeb"
  rm -f "$tmpdeb"
fi

echo "==> Setup complete"
R --version | head -1
quarto --version
