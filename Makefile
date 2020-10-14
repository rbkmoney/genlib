REBAR ?= $(shell which rebar3 2>/dev/null || which ./rebar3)

.PHONY: all compile dc test clean distclean dialyze check_format format

all: compile

compile:
	$(REBAR) compile

test:
	$(REBAR) do eunit, proper

clean:
	$(REBAR) clean

distclean:
	$(REBAR) clean -a

xref:
	$(REBAR) xref

dialyze:
	$(REBAR) dialyzer

check_format:
	$(REBAR) fmt -c

format:
	$(REBAR) fmt -w
