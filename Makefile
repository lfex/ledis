LFE_EBIN = ./deps/lfe/ebin
EREDIS_EBIN = ./deps/eredis/ebin

get-deps:
	rebar get-deps

compile: get-deps
	rebar compile

shell:
	./deps/lfe/bin/lfe -pa $(LFE_EBIN) -pa $(EREDIS_EBIN)