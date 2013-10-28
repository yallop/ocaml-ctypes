.SECONDEXPANSION:

OCAML=ocaml
OCAMLDEP=ocamldep
OCAMLFIND=ocamlfind
OCAMLMKLIB=ocamlmklib
VPATH=src examples
BUILDDIR=_build
PROJECTS=configure ctypes ctypes-foreign ctypes-top fts date ncurses
GENERATED=src/ctypes_config.h src/ctypes_config.ml setup.data src/ctypes/ctypes_primitives.ml
CFLAGS=-fPIC -Wall -O3 $(OCAML_FFI_INCOPTS)
OCAML_FFI_INCOPTS=$(patsubst "%",%,$(libffi_opt))

# public targets
all: setup.data build

build: $(PROJECTS)

clean:
	rm -fr _build

distclean: clean
	rm -f $(GENERATED)

# ctypes subproject
ctypes.public = unsigned signed structs ctypes posixTypes
ctypes.dir = src/ctypes
ctypes.extra_mls = ctypes_primitives.ml
ctypes.deps = bigarray
ctypes.install = yes

ctypes: PROJECT=ctypes
ctypes: $(ctypes.dir)/$(ctypes.extra_mls) $$(LIB_TARGETS)

# ctypes-foreign subproject
ctypes-foreign.public = dl foreign
ctypes-foreign.install = yes
ctypes-foreign.threads = yes
ctypes-foreign.dir = src/ctypes-foreign
ctypes-foreign.subproject_deps = ctypes
ctypes-foreign.link_flags = $(libffi_lib:"%"=%)
ctypes-foreign.cmo_opts = $(OCAML_FFI_INCOPTS:%=-ccopt %)
ctypes-foreign.cmx_opts = $(OCAML_FFI_INCOPTS:%=-ccopt %)

ctypes-foreign: PROJECT=ctypes-foreign
ctypes-foreign: $$(LIB_TARGETS)

# ctypes-top subproject
ctypes-top.public = ctypes_printers
ctypes-top.dir = src/ctypes-top
ctypes-top.install = yes
ctypes-top.deps = compiler-libs
ctypes-top.subproject_deps = ctypes

ctypes-top: PROJECT=ctypes-top
ctypes-top: $$(LIB_TARGETS)

# configure subproject
configure.dir = src/configure

configure: PROJECT=configure
configure: $$(NATIVE_TARGET)

# configuration
src/ctypes/ctypes_primitives.ml: $(BUILDDIR)/configure.native
	$< > $@

setup.data: src/discover/discover.ml
	ocaml $^ -ocamlc "$(OCAMLFIND) ocamlc"

# dependencies
depend: configure
	$(OCAMLDEP) $(foreach project,$(PROJECTS),-I $($(project).dir)) \
             src/*/*.mli src/*/*.ml examples/*/*.mli examples/*/*.ml \
           | sed "s!src/!_build/src/!g; s!examples/!_build/examples/!g" > .depend

#installation
META-install:
	$(OCAMLFIND) install ctypes META

install-%: PROJECT=$*
install-%:
	$(if $(filter yes,$($(PROJECT).install)),\
		$(OCAMLFIND) install -add ctypes $^ \
                   $(LIB_TARGETS) $(LIB_TARGET_EXTRAS) \
                   $(INSTALL_MLIS) $(INSTALL_CMIS) \
                   $(NATIVE_OBJECTS))

install: META-install $(PROJECTS:%=install-%)

uninstall:
	$(OCAMLFIND) remove ctypes

.PHONY: depend distclean clean build configure all install $(PROJECTS)

include .depend Makefile.rules Makefile.examples Makefile.tests
-include setup.data

