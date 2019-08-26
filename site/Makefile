start:
	yarn develop

open:
	open http://127.0.0.1:8000
open.gql.gatsby:
	open http://127.0.0.1:8000/__graphiql

build:
	gatsby build --prefix-paths

deploy: build
	yarn vars serverless client deploy --stage {{STAGE}} --region {{AWS_REGION}} --no-confirm

.PHONY: build deploy
