DEPS = ./deps
LFE_DIR = $(DEPS)/lfe
LFE_EBIN = $(LFE_DIR)/ebin
LFE = $(LFE_DIR)/bin/lfe
LFEC = $(LFE_DIR)/bin/lfec
EREDIS_DIR = ./deps/eredis
EREDIS_EBIN = $(EREDIS_DIR)/ebin
ERL_LIBS = $(LFE_DIR):$(EREDIS_DIR):./
SOURCE_DIR = ./src
OUT_DIR = ./ebin
TEST_DIR = ./test
TEST_OUT_DIR = ./.eunit

get-deps:
	rebar get-deps
	for DIR in $(wildcard $(DEPS)/*); do \
	cd $$DIR; git pull; cd - ; done

clean-ebin:
	-rm -f $(OUT_DIR)/*.beam

clean-eunit:
	-rm -rf $(TEST_OUT_DIR)

compile: get-deps clean-ebin
	rebar compile
	ERL_LIBS=$(ERL_LIBS) $(LFEC) -o $(OUT_DIR) $(SOURCE_DIR)/*.lfe

compile-tests: clean-eunit
	mkdir -p $(TEST_OUT_DIR)
	ERL_LIBS=$(ERL_LIBS) $(LFEC) -o $(TEST_OUT_DIR) $(TEST_DIR)/*_tests.lfe

shell:
	ERL_LIBS=$(ERL_LIBS) $(LFE) -pa $(TEST_OUT_DIR)

clean: clean-ebin clean-eunit
	rebar clean

# XXX if a unit test fails, erl ... "eunit:test..." still exits with status code
# 0 and thus the next command that may depend upon the check target will
# execute. This is not good.
check: TEST_MODS = $(wildcard $(TEST_OUT_DIR)/*.beam)
check: compile compile-tests
	@#rebar eunit verbose=1 skip_deps=true
	@clear;
	@for FILE in $(TEST_MODS); do \
	F1="$$(basename $$FILE)"; F2=$${F1%.*}; \
	echo $$F2; done|sed -e :a -e '$$!N; s/\n/,/; ta' | \
	ERL_LIBS=$(ERL_LIBS) \
	xargs -I % erl -W0 -pa $(TEST_OUT_DIR) -noshell \
	-eval "eunit:test([%], [verbose])" \
	-s init stop

push:
	git push oubiwann master

push-all: push
	git push --all