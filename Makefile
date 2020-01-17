all: install

start:
	@yarn -s x api start

vars:
	@yarn -s vars -V

install:
	@./node_modules/typescript/bin/tsc -b && \
		yarn -s vars yarn -s configure up -v
force:
	@yarn -s vars yarn -s configure up -v -f

clean:
	@yarn -s configure clean

sync:
	@tsc -b && cd ./mobile && \
		cp -r ../packages/example/lib/ node_modules/example/lib/
