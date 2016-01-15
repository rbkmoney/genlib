REBAR ?= $(shell which rebar3 2>/dev/null || which ./rebar3)

.PHONY: all compile dc test clean distclean dialyze

all: compile

dc:
compile:
	$(REBAR) compile

test:
	$(REBAR) eunit

clean:
	$(REBAR) clean

distclean:
	$(REBAR) clean -a

dialyze:
	$(REBAR) dialyzer
