ERL          ?= erl
EBIN_DIRS    := $(wildcard deps/*/ebin)

all: exmpp lhttpc yaws erl

erl:
	mkdir -p ebin
	@$(ERL) -pa $(EBIN_DIRS) -noinput +B \
	  -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

exmpp:
	cd deps/exmpp && (test -f Makefile || (autoreconf -vif && ./configure --disable-documentation)) && $(MAKE)

yaws:
	cd deps/yaws && (test -f configure|| (autoconf && ./configure)) && $(MAKE)

lhttpc:
	cd deps/lhttpc && $(MAKE)
docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

clean: 
	@echo "removing:"
	@rm -fv ebin/*.beam ebin/*.app
