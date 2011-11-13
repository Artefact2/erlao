CC?=clang
VERSION=$(shell git describe --match "v*" 2>/dev/null | sed -e "s/^v//")

default: beamfiles port

all: beamfiles port docs

beamfiles:
	@erl -make

port: priv/erlao_port

docs:
	@erl -noshell -env ERL_LIBS "." -eval "halt(case edoc:application(erlao, \".\", []) of ok -> 0; _ -> 1 end)."

clean:
	@rm -f ebin/*.beam priv/erlao_port
	@rm -Rf doc

tarball_source: erlao-$(VERSION).tar

erlao-$(VERSION).tar:
	@git archive --prefix=erlao-$(VERSION)/ -o erlao-$(VERSION).tar v$(VERSION)
	@echo $@

priv/erlao_port: priv/erlao_port.c
	$(CC) $(CFLAGS) -Wall -Wextra -Werror -pedantic -pedantic-errors -lao -ldl -o $@ $<

.PHONY: beamfiles docs default clean tarball_source