MAKEFILE_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

GEN = $(MAKEFILE_DIR)/gen
SEED = $(MAKEFILE_DIR)/seed

configure:
	@$(GEN) generate && \
		$(GEN) seed $(DB_URL) $(SERVICE_ID)

.PHONY: configure
