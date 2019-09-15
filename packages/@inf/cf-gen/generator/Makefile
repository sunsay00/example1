ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

configure:
	@cd $(ROOT_DIR) && rm -rf $(ROOT_DIR)/front $(ROOT_DIR)/back $(ROOT_DIR)/infra && csi -s $(ROOT_DIR)/main.scm $(LEDGER_PATH)

configure.client:
	cd $(ROOT_DIR) && csi -s $(ROOT_DIR)/main-client.scm $(LEDGER_PATH) $(OUTPUT_PATH)
	cp -r $(ROOT_DIR)/../client-tools $(OUTPUT_PATH)/tools

