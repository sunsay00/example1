test:
	@yarn -s ss test test $(ARGS)

start:
	@yarn -s ss test start

invoke:
	@yarn -s ss test invoke.test "SELECT (1 + 1); SELECT (2 + 2)"

logs:
	@yarn -s vars sls logs -s {{STAGE}} -f invoke -t -r {{AWS_REGION}}
