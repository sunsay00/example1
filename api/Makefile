test:
	yarn vars yarn test

invoke:
	yarn vars curl -H 'Authorization: Poopy' -H 'Content-Type: application/json' -d "hello world" {{CF_API_GraphQLEndpoint}}
invoke.guest:
	yarn vars curl -H 'Authorization: Guest' -H 'content-type: application/json' -d '{"query":"query($$arg: String!) { testUnauthorized (arg: $$arg) }","variables": { "arg": "pong" }}' {{CF_API_GraphQLEndpoint}}

logs.auth:
	yarn vars sls logs -s {{STAGE}} -f auth -t -r {{AWS_REGION}}
logs.api:
	yarn vars sls logs -s {{STAGE}} -f api -t -r {{AWS_REGION}}

configure:
	rm -f tsconfig.tsbuildinfo && tsc && yarn webpack && cp build/_api.js build/api.js && cp build/_auth.js build/auth.js && \
		yarn vars yarn sls deploy --no-confirm
