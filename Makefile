all: configure

.envs/aws: envs
	@git submodule update --init --recursive && tsc -b && \
		mkdir -p .envs && yarn vars yarn awsinfo .envs/aws

vars:
	yarn vars -V

configure: .envs/aws
	yarn vars yarn configure up
