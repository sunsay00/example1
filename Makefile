all: configure

start:
	@yarn -s ss api start

vars:
	@yarn -s vars -V

configure:
	@yarn -s vars yarn -s configure up -v

clean:
	@yarn -s configure clean

sync:
	@tsc -b && cd ./mobile && \
		cp -r ../packages/@inf/common/lib/ node_modules/@inf/common/lib/ && \
		cp -r ../packages/@inf/core-ui/lib/ node_modules/@inf/core-ui/lib/ && \
		cp -r ../packages/@inf/apollo/lib/ node_modules/@inf/apollo/lib/ && \
		cp -r ../packages/@inf/cf-cognito/lib/ node_modules/@inf/cf-cognito/lib/ && \
		cp -r ../packages/@inf/mobile-ui/lib/ node_modules/@inf/mobile-ui/lib/

