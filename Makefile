include envs

all: configure

.envs.$(STAGE)/aws: envs
	@git submodule update --init --recursive && tsc -b && \
		mkdir -p .envs.$(STAGE) && yarn vars yarn awsinfo .envs.$(STAGE)/aws

vars:
	yarn vars -V

configure: .envs.$(STAGE)/aws
	yarn vars yarn configure up
