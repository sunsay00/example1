TS_FILES = $(shell find ./src -regex ".*\.ts$$")

build/_api.js build/_auth.js: $(TS_FILES)
	rm -f tsconfig.tsbuildinfo && tsc && yarn webpack && cp build/_api.js build/api.js && cp build/_auth.js build/auth.js

.envs.lambda: build/_api.js build/_auth.js
	yarn vars yarn sls deploy -v 2>&1 | yarn lambdainfo .envs.lambda

vars:
	yarn vars -V

configure: .envs.lambda
