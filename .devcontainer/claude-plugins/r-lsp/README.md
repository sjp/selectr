# r-lsp

Registers the [`languageserver`](https://cran.r-project.org/package=languageserver)
CRAN package as Claude Code's LSP server for `.R` files, so the `LSP` tool can
answer go-to-definition, find-references, hover and symbol queries in this
package.

`.devcontainer/post-create.sh` installs `languageserver` and enables this plugin;
there is nothing to configure by hand.
