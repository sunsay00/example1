test:
	yarn ss api test

start:
	yarn ss api start

invoke:
	yarn ss api invoke.api
invoke.guest:
	yarn ss api invoke.api.guest

logs.auth:
	yarn vars sls logs -s {{STAGE}} -f auth -t -r {{AWS_REGION}}
logs.api:
	yarn vars sls logs -s {{STAGE}} -f api -t -r {{AWS_REGION}}
