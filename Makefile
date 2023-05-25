.PHONY: lint distclean

# TODO: consider splitting this into two steps:
# - generate classpath (depends on deps.edn)
# - clj-kondo init (depends on classpath and .clj-kondo/config.edn)
.clj-kondo/.cache/INITIALIZED: .clj-kondo/config.edn deps.edn
	$(dir $(lastword $(MAKEFILE_LIST)))bin/init-clj-kondo
	touch .clj-kondo/.cache/INITIALIZED

lint: .clj-kondo/.cache/INITIALIZED
	$(dir $(lastword $(MAKEFILE_LIST)))bin/lint

distclean:
	rm -rf .clj-kondo/.cache
