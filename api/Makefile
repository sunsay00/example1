TS_FILES = $(shell find ./src -regex ".*\.ts$$")

build/_api.js build/_auth.js: $(TS_FILES)
	rm -f tsconfig.tsbuildinfo && tsc && yarn webpack && cp build/_api.js build/api.js && cp build/_auth.js build/auth.js

.envs.lambda: build/_api.js build/_auth.js
	yarn vars yarn sls deploy

vars:
	yarn vars -V

invoke:
	#yarn vars curl -v -H 'Authorization: Guest' -d "hello world" {{CF_API_GraphQLEndpoint}}
	yarn vars curl -v -H 'Authorization: Poopy' -d "hello world" {{CF_API_GraphQLEndpoint}}

logs.auth:
	yarn vars sls logs -s {{STAGE}} -f auth -t -r {{AWS_REGION}}
logs.api:
	yarn vars sls logs -s {{STAGE}} -f api -t -r {{AWS_REGION}}

#configure: .envs.lambda
configure:
	rm -f tsconfig.tsbuildinfo && tsc && yarn webpack && cp build/_api.js build/api.js && cp build/_auth.js build/auth.js && \
		yarn vars yarn sls deploy --no-confirm
