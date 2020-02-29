.PHONY: list
list: ## Show available targets.
	@fgrep -h "##" $(MAKEFILE_LIST) | fgrep -v fgrep | sed -e 's/\\$$//' | sed -e 's/##\s*\(.*\)/\n\t\1\n/'

.PHONY: watch
watch: ## Compile files on change and run a hotswapping server
	@spago build --watch

.PHONY: clean
clean: ## Remove all generated project files (keeps standard library).
	@find src -type f | cut -d/ -f2- | cut -d. -f1 | sed 's/\//./g' | sed 's/^/output\//' | xargs -L1 rm -rf
	@rm -rf dist/

.PHONY: build
build: ## Build source files
	@spago build

.PHONY: tags
tags: build ## Create machine-readable project documentation.
	@spago sources | xargs purs docs --format ctags

.PHONY: package
package: ## Build source files and bundle into browser usable package
	@mkdir dist
	@cp data/* dist/
	@npm run bundle
