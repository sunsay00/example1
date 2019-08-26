MAKEFILE_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

GEN = $(MAKEFILE_DIR)/gen
SEED = $(MAKEFILE_DIR)/seed

configure:
	$(GEN)
	$(SEED) $(DBCONNSTR) $(SERVICEID)

.PHONY: configure