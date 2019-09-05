include _vars.env

ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

# DESC: spins up spot instance
up:
	@yarn -s vars ts-node -T $(ROOT_DIR)/src/spot up

# DESC: spins down spot instance
down:
	@yarn -s vars ts-node -T $(ROOT_DIR)/src/spot down

# DESC: SSH into instance
shell:
	@$(eval PublicDnsName := $(shell yarn -s vars aws --region {{AWS_REGION}} ec2 describe-instances | jq '.Reservations[].Instances[] | select(.KeyName=="$(PUB_KEY)" and .State.Name=="running") | .PublicDnsName'))
	@yarn -s vars ./bin/startshell $(SHELL_USER) $(PublicDnsName)