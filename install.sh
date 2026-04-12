#!/usr/bin/env bash
set -euo pipefail

GREEN="\033[1;32m"
RED="\033[1;31m"
CYAN="\033[1;36m"
BOLD="\033[1m"
RESET="\033[0m"

ok()   { echo -e "  ${GREEN}✔${RESET}  $*"; }
err()  { echo -e "  ${RED}✘${RESET}  $*" >&2; exit 1; }
info() { echo -e "  ${CYAN}→${RESET}  $*"; }

REPO="Miguevrgo/Zeru"
ZERU_HOME="${HOME}/.zeru"
ZERU_BIN="${ZERU_HOME}/bin"
ZERU_STD="${ZERU_HOME}/std"

echo ""
echo -e "  ${BOLD}Zeru Installer${RESET}"
echo ""

# Create directories
info "Setting up ${ZERU_HOME}..."
mkdir -p "${ZERU_BIN}" "${ZERU_STD}"
ok "Directories ready"

# Detect AVX2 for optimal binary
BINARY="zeru-linux-generic"
if grep -q avx2 /proc/cpuinfo 2>/dev/null; then
    BINARY="zeru-linux-avx"
fi
info "Binary variant: ${BINARY}"

# Resolve latest release tag via GitHub redirect (no jq required)
LATEST=$(curl -fsSL -o /dev/null -w '%{url_effective}' \
    "https://github.com/${REPO}/releases/latest" | sed 's|.*/||') \
    || err "Failed to resolve latest release"
ok "Latest release: ${LATEST}"

BASE_URL="https://github.com/${REPO}/releases/download/${LATEST}"

# Download binary
info "Downloading binary..."
curl -fsSL --progress-bar "${BASE_URL}/${BINARY}" -o "${ZERU_BIN}/zeru" \
    || err "Failed to download binary"
chmod +x "${ZERU_BIN}/zeru"
ok "Binary installed to ${ZERU_BIN}/zeru"

# Download and extract std library
info "Downloading std library..."
curl -fsSL --progress-bar "${BASE_URL}/zeru-std.tar.gz" \
    | tar -xz -C "${ZERU_STD}" --strip-components=1 \
    || err "Failed to download std library"
ok "Std library installed to ${ZERU_STD}"

# Write install manifest
cat > "${ZERU_HOME}/config.toml" << EOF
[zeru]
version = "${LATEST}"

[std]
path = "${ZERU_STD}"
EOF
ok "Config written to ${ZERU_HOME}/config.toml"

echo ""
echo -e "  ${GREEN}${BOLD}Zeru ${LATEST} installed successfully${RESET}"
echo ""
echo -e "  Add to your shell profile (~/.bashrc, ~/.zshrc, etc.):"
echo ""
echo -e "    ${BOLD}export PATH=\"\${HOME}/.zeru/bin:\${PATH}\"${RESET}"
echo ""
