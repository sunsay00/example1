TS_FILES = $(shell find ./src -regex ".*\.ts$$")

build/_api.js build/_auth.js: $(TS_FILES)
	rm -f tsconfig.tsbuildinfo && tsc && yarn webpack && cp build/_api.js build/api.js && cp build/_auth.js build/auth.js

.envs.lambda: build/_api.js build/_auth.js
	yarn vars yarn sls deploy

vars:
	yarn vars -V

invoke:
	curl -H 'Authorization: Bearer' -d "hello world" https://b5a2mkvqbj.execute-api.us-east-1.amazonaws.com/dev

logs.auth:
	yarn vars sls logs -s $(STAGE) -f auth -t -r $(AWS_REGION)
logs.api:
	yarn vars sls logs -s $(STAGE) -f api -t -r $(AWS_REGION)

configure: .envs.lambda
