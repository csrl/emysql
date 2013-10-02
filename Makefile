VERSION=1.0.0

EBIN_DIR=ebin
SRC_DIR=src
INC_DIR=include

SOURCES=$(wildcard $(SRC_DIR)/*.erl)
OBJECTS=$(SOURCES:$(SRC_DIR)/%.erl=$(EBIN_DIR)/%.beam)
APP=$(EBIN_DIR)/emysql.app

ERLC_FLAGS=-I $(INC_DIR)

ifdef DEBUG_INFO
	ERLC_FLAGS += +debug_info
endif

ifdef DEBUG
	ERLC_FLAGS += -Ddebug
endif

MODULES=$(shell ls -1 src/*.erl | awk -F[/.] '{ print $$2 }' | sed '$$q;s/$$/,/g')
LIBDIR=$(shell erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell)

all: $(APP) $(OBJECTS)

$(EBIN_DIR)/%.beam: $(SRC_DIR)/%.erl
	erlc $(ERLC_FLAGS) -o $(EBIN_DIR) $<

clean:
	rm -f $(OBJECTS) $(APP)

$(APP): $(SRC_DIR)/emysql.app.src
	mkdir -p ebin
	@sed -e 's/{modules, \[\]}/{modules, [$(MODULES)]}/' -e 's/{vsn, ""}/{vsn, "$(VERSION)"}/' < $< > $@

install:
	@for i in $(EBIN_DIR)/*.beam $(EBIN_DIR)/*.app include/*.hrl $(SRC_DIR)/*.erl; do install -m 644 -D $$i $(prefix)/$(LIBDIR)/emysql-$(VERSION)/$$i ; done
