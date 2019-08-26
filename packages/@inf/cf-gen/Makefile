MAKEFILE_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

GEN = $(MAKEFILE_DIR)/gen
SEED = $(MAKEFILE_DIR)/seed

configure:
	$(GEN) && \
		$(SEED) $(DBCONNSTR) $(SERVICEID) && \
		DB=$(DBCONNSTR) yarn db-migrate up --migrations-dir $(MAKEFILE_DIR)/infra/postgres/migrations --config $(MAKEFILE_DIR)/database.json

.PHONY: configure
