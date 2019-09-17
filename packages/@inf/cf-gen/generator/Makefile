ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

frontend:
	@cd $(ROOT_DIR) && csi -s $(ROOT_DIR)/main-frontend.scm $(LEDGER_PATH) $(OUTPUT_PATH)

backend:
	@cd $(ROOT_DIR) && csi -s $(ROOT_DIR)/main-backend.scm $(LEDGER_PATH) $(OUTPUT_PATH) && \
		cp $(ROOT_DIR)/../index.backend.ts_ $(OUTPUT_PATH)/index.ts && \
		cp $(ROOT_DIR)/../grant.ts_ $(OUTPUT_PATH)/grant.ts && \
		cp $(ROOT_DIR)/../resolver.ts_ $(OUTPUT_PATH)/resolver.ts && \
		cp $(ROOT_DIR)/../customtypes.ts_ $(OUTPUT_PATH)/customtypes.ts && \
		cp $(ROOT_DIR)/../smoketester.ts_ $(OUTPUT_PATH)/smoketester.ts && \
		cp $(ROOT_DIR)/../database.json_ $(OUTPUT_PATH)/database.json

