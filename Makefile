.SECONDEXPANSION:

OCAML=ocaml
OCAMLDEP=ocamldep
OCAMLFIND=ocamlfind
OCAMLMKLIB=ocamlmklib
VPATH=src examples
BUILDDIR=_build
PROJECTS=configure configured ctypes ctypes-foreign-base ctypes-foreign-threaded ctypes-foreign-unthreaded ctypes-top fts date ncurses
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

# ctypes-foreign-base subproject
ctypes-foreign-base.public = dl
ctypes-foreign-base.install = yes
ctypes-foreign-base.threads = no
ctypes-foreign-base.dir = src/ctypes-foreign-base
ctypes-foreign-base.subproject_deps = ctypes
ctypes-foreign-base.link_flags = $(libffi_lib:"%"=%)
ctypes-foreign-base.cmo_opts = $(OCAML_FFI_INCOPTS)
ctypes-foreign-base.cmx_opts = $(OCAML_FFI_INCOPTS)

ctypes-foreign-base: PROJECT=ctypes-foreign-base
ctypes-foreign-base: $$(LIB_TARGETS)

# ctypes-foreign-threaded subproject
ctypes-foreign-threaded.public = foreign
ctypes-foreign-threaded.install = yes
ctypes-foreign-threaded.threads = yes
ctypes-foreign-threaded.dir = src/ctypes-foreign-threaded
ctypes-foreign-threaded.subproject_deps = ctypes ctypes-foreign-base
ctypes-foreign-threaded.link_flags = $(libffi_lib:"%"=%)
ctypes-foreign-threaded.cmo_opts = $(OCAML_FFI_INCOPTS)
ctypes-foreign-threaded.cmx_opts = $(OCAML_FFI_INCOPTS)

ctypes-foreign-threaded: PROJECT=ctypes-foreign-threaded
ctypes-foreign-threaded: $$(LIB_TARGETS)

# ctypes-foreign-unthreaded subproject
ctypes-foreign-unthreaded.public = foreign
ctypes-foreign-unthreaded.install = yes
ctypes-foreign-unthreaded.threads = no
ctypes-foreign-unthreaded.dir = src/ctypes-foreign-unthreaded
ctypes-foreign-unthreaded.subproject_deps = ctypes ctypes-foreign-base
ctypes-foreign-unthreaded.link_flags = $(libffi_lib:"%"=%)
ctypes-foreign-unthreaded.cmo_opts = $(OCAML_FFI_INCOPTS)
ctypes-foreign-unthreaded.cmx_opts = $(OCAML_FFI_INCOPTS)

ctypes-foreign-unthreaded: PROJECT=ctypes-foreign-unthreaded
ctypes-foreign-unthreaded: $$(LIB_TARGETS)

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
configured: src/ctypes/ctypes_primitives.ml

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

