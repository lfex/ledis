LFE_DIR = ./deps/lfe
LFE_EBIN = $(LFE_DIR)/ebin
LFE = $(LFE_DIR)/bin/lfe
LFEC = $(LFE_DIR)/bin/lfec
EREDIS_DIR = ./deps/eredis
EREDIS_EBIN = $(EREDIS_DIR)/ebin
ERL_LIBS = $(LFE_DIR):$(EREDIS_DIR):./
OUT_DIR = ./ebin

get-deps:
	rebar get-deps

clean-ebin:
	-rm $(OUT_DIR)/*.beam

compile: get-deps clean-ebin
	rebar compile
	ERL_LIBS=$(ERL_LIBS) $(LFEC) -o $(OUT_DIR) src/*.lfe

shell:
	ERL_LIBS=$(ERL_LIBS) $(LFE)

clean: clean-ebin
	rebar clean

check:
	rebar eunit skip_deps=true verbose=1