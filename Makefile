.DEFAULT_GOAL := help

################################################################################
# Server

serve: ## Launch the server.
	cabal run --ghc-options="-O0" -- csdc-server config.json

.PHONY: serve

################################################################################
# GUI

gui-build: ## Build the GUI into www/app.js.
	mkdir -p www
	cd csdc-gui && elm make src/Main.elm --output ../www/app.js

.PHONY: gui-build

gui-build-optimized: ## Build the GUI into www/app.min.js
	mkdir -p www
	cd csdc-gui && elm make --optimize src/Main.elm --output ../www/app.js
	uglifyjs www/app.js \
	  --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
		| uglifyjs --mangle --output=www/app.min.js

.PHONY: gui-build-optimized

################################################################################
# Haskell development

ghcid-api: ## Launch ghcid for csdc-api.
	ghcid --command "cabal repl csdc-api"

.PHONY: ghcid-api

ghcid-base: ## Launch ghcid for csdc-base.
	ghcid --command "cabal repl csdc-base"

.PHONY: ghcid-base

ghcid-server: ## Launch ghcid for the server executable.
	ghcid --command "cabal repl csdc-api:csdc-server"

.PHONY: ghcid-server

hoogle: ## Launch the Hoogle server
	hoogle server --local -p 8888

.PHONY: hoogle

################################################################################
# Elm development

elm-nix-update: ## Update Nix files for Elm.
	cd csdc-gui && elm2nix convert > elm-srcs.nix && elm2nix snapshot

.PHONY: elm-nix-update

elm-docs: ## Elm Docs
	edp csdc-gui

.PHONY: elm-docs

################################################################################
# Database

psql: ## Launch psql with the correct arguments.
	PGHOST=localhost PGPORT=5432 PGDATABASE=csdc PGUSER=csdc PGPASSWORD=csdc psql

.PHONY: psql

################################################################################
# Docker

docker: ## Create and start the Docker image.
	docker-compose --file database/postgresql.yml build
	touch .docker
	docker-compose --file database/postgresql.yml up -d

.PHONY: docker

docker-clean: ## Clean the Docker image.
	docker-compose --file database/postgresql.yml down -v
	rm -f .docker

.PHONY: docker-clean

################################################################################
# Help

help: ## Print the list of commands.
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: help
