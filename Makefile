LOAD_PATH = \
	ebin \
	$(NULL)

MNESIA_DIR = /tmp/erlq.db

NAME = erlq
HOST = `hostname`
NODE = $(NAME)@$(HOST)

BASIC_OPTS = \
	-pa $(LOAD_PATH) \
	+A 8 +K true +P 120000 -smp disable \
	$(NULL)

all: compile

compile:
	erl -make +debug_info -smp disable

make_boot: compile
	erl $(BASIC_OPTS) -s erlq_admin make_boot -s init stop

run: compile
	erl $(BASIC_OPTS) -name $(NODE) -s erlq start

clean:
	rm -rf ebin/*.beam
	rm -fr $(MNESIA_DIR)



