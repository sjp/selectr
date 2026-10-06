#!/usr/bin/env bash
set -euo pipefail

claude_dir=${CLAUDE_CONFIG_DIR:-$HOME/.claude}
claude_json=${CLAUDE_CONFIG_DIR:-$HOME}/.claude.json

# The volume is root-owned on first creation, update to the container user.
mkdir -p "$claude_dir"
if [ "$(stat -c %u "$claude_dir")" != "$(id -u)" ]; then
    sudo chown -R "$(id -u):$(id -g)" "$claude_dir"
fi

# Skip onboarding and the per-folder trust dialog. Merge rather than overwrite.
claude_config=$(jq -n --arg dir "$PWD" '{
    hasCompletedOnboarding: true,
    projects: { ($dir): { hasTrustDialogAccepted: true } }
}')
if [ -f "$claude_json" ]; then
    jq --argjson add "$claude_config" '. * $add' "$claude_json" > "$claude_json.tmp"
else
    printf '%s\n' "$claude_config" > "$claude_json.tmp"
fi
mv "$claude_json.tmp" "$claude_json"

# The claude-code feature installs the package as root-owned, so
# in-place auto-updates fail with "no_permissions". Hand it to the container user.
npm_root=$(npm root -g)
if [ -d "$npm_root/@anthropic-ai" ]; then
    sudo chown -R "$(id -u):$(id -g)" "$npm_root/@anthropic-ai"
fi

# An R language server, so Claude Code's LSP tool and the VS Code R extension
# can resolve symbols in the package. Debian has no r-cran-languageserver, but
# it does package most of its dependencies; only collections, styler, the R.*
# family and languageserver itself are left to build from source.
languageserver_apt_deps=(
    r-cran-brew r-cran-callr r-cran-cli r-cran-codetools r-cran-commonmark
    r-cran-cpp11 r-cran-desc r-cran-digest r-cran-evaluate r-cran-fs
    r-cran-glue r-cran-highr r-cran-jsonlite r-cran-knitr r-cran-lifecycle
    r-cran-lintr r-cran-magrittr r-cran-otel r-cran-pkgbuild r-cran-pkgload
    r-cran-processx r-cran-ps r-cran-purrr r-cran-r6 r-cran-rdtools r-cran-rex
    r-cran-rlang r-cran-roxygen2 r-cran-rprojroot r-cran-stringi r-cran-vctrs
    r-cran-withr r-cran-xfun r-cran-xml2 r-cran-xmlparsedata r-cran-yaml
)
sudo apt-get update -qq
sudo DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
    "${languageserver_apt_deps[@]}"
Rscript -e 'if (!requireNamespace("languageserver", quietly = TRUE))
    install.packages("languageserver", Ncpus = parallel::detectCores())'

# Claude Code only takes LSP servers from plugins, so register the one-plugin
# marketplace in .devcontainer/claude-plugins and install the plugin from it.
# Both commands are no-ops once they have run.
#
# The claude-code-config volume is shared with other devcontainers, so this
# directory may already be registered under another marketplace name (e.g. one
# copied from a different project). `marketplace add` then keeps the old name
# and the install below can't find selectr-devcontainer, so drop those first.
marketplace_dir="$PWD/.devcontainer/claude-plugins"
marketplace_name=$(jq -r .name "$marketplace_dir/.claude-plugin/marketplace.json")
claude plugin marketplace list --json |
    jq -r --arg dir "$marketplace_dir" --arg name "$marketplace_name" \
        '.[] | select(.path == $dir and .name != $name) | .name' |
    while read -r stale; do
        claude plugin marketplace remove "$stale"
    done
claude plugin marketplace add "$marketplace_dir"
claude plugin install "r-lsp@$marketplace_name"
