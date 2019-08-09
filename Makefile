include envs

all: configure

.envs.$(STAGE)/aws: envs
	@git submodule update --init --recursive && tsc -b && \
		mkdir -p .envs.$(STAGE) && yarn vars yarn awsinfo .envs.$(STAGE)/aws

vars:
	yarn vars -V

configure: .envs.$(STAGE)/aws
	yarn vars yarn configure up

sync:
	tsc -b && cd ./mobile && \
		cp -r ../packages/common/lib/ node_modules/common/lib/ && \
		cp -r ../packages/core-ui/lib/ node_modules/core-ui/lib/ && \
		cp -r ../packages/cf-cognito/lib/ node_modules/cf-cognito/lib/ && \
		cp -r ../packages/mobile-ui/lib/ node_modules/mobile-ui/lib/

