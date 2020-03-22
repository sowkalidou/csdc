.DEFAULT_GOAL := help

################################################################################
# Haskell development

ghcid-api: ## Launch ghcid for csdc-api.
	ghcid --command "cabal new-repl csdc-api"

.PHONY: ghcid-api

ghcid-base: ## Launch ghcid for csdc-base.
	ghcid --command "cabal new-repl csdc-base"

.PHONY: ghcid-base

################################################################################
# Elm development

elm-reload: ## Update Nix files for Elm.
  cd csdc-gui && elm2nix convert > elm-srcs.nix

.PHONY: elm-reload

################################################################################
# Help

help: ## Print the list of commands.
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: help
