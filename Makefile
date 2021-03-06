REBAR=`which rebar || printf ./rebar`

LOAD_PATH = \
	ebin \
	$(NULL)

NAME = erlq
HOST = `hostname`
NODE = $(NAME)@$(HOST)

BASIC_OPTS = \
	-pa $(LOAD_PATH) \
	+A 8 +K true +P 120000 -smp disable \
	$(NULL)

all: get-deps compile

get-deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

ct:
	@$(REBAR) skip_deps=true ct

eunit:
	@$(REBAR) skip_deps=true eunit

test: eunit ct

run: compile
	erl $(BASIC_OPTS) -name $(NODE) -s erlq start

clean:
	@$(REBAR) clean
