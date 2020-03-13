.PHONY: help install build test
.DEFAULT_GOAL := help

######################################################
#### Utils
######################################################

help: ## Ask for help!
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

######################################################
#### Init
######################################################
install: ## install dependencies
	yarn && \
	yarn spago install

######################################################
#### Build
######################################################
build: ## build purs modules
	yarn spago build

######################################################
#### Test
######################################################
test: ## run tests
	make deploy-test-chain && \
	sleep 3 && \
	yarn spago test; \
	make takedown-test-chain;

deploy-test-chain: ## Deploys the test chain
	docker-compose -p hd-wallet-provider up -d

takedown-test-chain: ## Removes chain containers and wipes volumes
	docker-compose -p hd-wallet-provider kill && \
	docker-compose -p hd-wallet-provider down -v