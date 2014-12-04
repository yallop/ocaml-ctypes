.SECONDEXPANSION:

DEBUG=false
OCAML=ocaml
OCAMLFIND=ocamlfind
OCAMLDEP=$(OCAMLFIND) ocamldep
OCAMLMKLIB=$(OCAMLFIND) ocamlmklib
VPATH=src examples
BUILDDIR=_build
PROJECTS=configure libffi-abigen configured ctypes cstubs ctypes-foreign-base ctypes-foreign-threaded ctypes-foreign-unthreaded ctypes-top
GENERATED=src/ctypes_config.h src/ctypes_config.ml setup.data src/ctypes/ctypes_primitives.ml src/ctypes-foreign-base/dl.ml src/ctypes-foreign-base/dl_stubs.c
OCAML_FFI_INCOPTS=$(libffi_opt)
export CFLAGS DEBUG

EXTDLL:=$(shell $(OCAMLFIND) ocamlc -config | awk '/^ext_dll:/{print $$2}')
OSYSTEM:=$(shell $(OCAMLFIND) ocamlc -config | awk '/^system:/{print $$2}')

ifneq (,$(filter mingw%,$(OSYSTEM)))
OS_ALT_SUFFIX=.win
else
OS_ALT_SUFFIX=.unix
endif

# public targets
all: setup.data build

build: $(PROJECTS)

clean:
	rm -fr _build

distclean: clean
	rm -f $(GENERATED)

# ctypes subproject
ctypes.public = static primitives unsigned signed structs ctypes posixTypes
ctypes.dir = src/ctypes
ctypes.extra_mls = ctypes_primitives.ml
ctypes.deps = str bigarray bytes
ctypes.install = yes
ctypes.install_native_objects = yes
ifeq ($(XEN),enable)
ctypes.xen = yes
endif

ctypes: PROJECT=ctypes
ctypes: $(ctypes.dir)/$(ctypes.extra_mls) $$(LIB_TARGETS)

# cstubs subproject
cstubs.public = cstubs_internals cstubs cstubs_inverted cstubs_structs
cstubs.dir = src/cstubs
cstubs.subproject_deps = ctypes
cstubs.deps = str bytes
cstubs.install = yes

cstubs: PROJECT=cstubs
cstubs: $(cstubs.dir)/$(cstubs.extra_mls) $$(LIB_TARGETS)

# ctypes-foreign-base subproject
ctypes-foreign-base.public = dl libffi_abi
ctypes-foreign-base.install = yes
ctypes-foreign-base.install_native_objects = yes
ctypes-foreign-base.threads = no
ctypes-foreign-base.dir = src/ctypes-foreign-base
ctypes-foreign-base.deps = bytes
ctypes-foreign-base.subproject_deps = ctypes
ctypes-foreign-base.extra_mls = libffi_abi.ml dl.ml
ctypes-foreign-base.extra_cs = dl_stubs.c
ctypes-foreign-base.link_flags = $(libffi_lib) $(lib_process)
ctypes-foreign-base.cmo_opts = $(OCAML_FFI_INCOPTS:%=-ccopt %)
ctypes-foreign-base.cmx_opts = $(OCAML_FFI_INCOPTS:%=-ccopt %)

ctypes-foreign-base: PROJECT=ctypes-foreign-base
ctypes-foreign-base: $$(LIB_TARGETS)

# ctypes-foreign-threaded subproject
ctypes-foreign-threaded.public = foreign
ctypes-foreign-threaded.install = yes
ctypes-foreign-threaded.threads = yes
ctypes-foreign-threaded.dir = src/ctypes-foreign-threaded
ctypes-foreign-threaded.subproject_deps = ctypes ctypes-foreign-base
ctypes-foreign-threaded.link_flags = $(libffi_lib) $(lib_process)
ctypes-foreign-threaded.cmo_opts = $(OCAML_FFI_INCOPTS:%=-ccopt %)
ctypes-foreign-threaded.cmx_opts = $(OCAML_FFI_INCOPTS:%=-ccopt %)
ctypes-foreign-threaded.install_native_objects = no

ctypes-foreign-threaded: PROJECT=ctypes-foreign-threaded
ctypes-foreign-threaded: $$(LIB_TARGETS)

# ctypes-foreign-unthreaded subproject
ctypes-foreign-unthreaded.public = foreign
ctypes-foreign-unthreaded.install = yes
ctypes-foreign-unthreaded.threads = no
ctypes-foreign-unthreaded.dir = src/ctypes-foreign-unthreaded
ctypes-foreign-unthreaded.subproject_deps = ctypes ctypes-foreign-base
ctypes-foreign-unthreaded.link_flags = $(libffi_lib) $(lib_process)
ctypes-foreign-unthreaded.cmo_opts = $(OCAML_FFI_INCOPTS:%=-ccopt %)
ctypes-foreign-unthreaded.cmx_opts = $(OCAML_FFI_INCOPTS:%=-ccopt %)
ctypes-foreign-unthreaded.install_native_objects = no

ctypes-foreign-unthreaded: PROJECT=ctypes-foreign-unthreaded
ctypes-foreign-unthreaded: $$(LIB_TARGETS)

# ctypes-top subproject
ctypes-top.public = ctypes_printers
ctypes-top.dir = src/ctypes-top
ctypes-top.install = yes
ctypes-top.deps = compiler-libs
ctypes-top.subproject_deps = ctypes
ctypes-top.install_native_objects = yes

ctypes-top: PROJECT=ctypes-top
ctypes-top: $$(LIB_TARGETS)

# configure subproject
configure.dir = src/configure

configure: PROJECT=configure
configure: $$(NATIVE_TARGET)

# libffi-abigen subproject
libffi-abigen.dir = src/libffi-abigen
libffi-abigen.install = no
libffi-abigen.deps = unix
libffi-abigen: PROJECT=libffi-abigen
libffi-abigen: $$(NATIVE_TARGET)

# configuration
configured: src/ctypes/ctypes_primitives.ml src/ctypes-foreign-base/libffi_abi.ml src/ctypes-foreign-base/dl.ml src/ctypes-foreign-base/dl_stubs.c

src/ctypes-foreign-base/dl.ml: src/ctypes-foreign-base/dl.ml$(OS_ALT_SUFFIX)
	cp $< $@
src/ctypes-foreign-base/dl_stubs.c: src/ctypes-foreign-base/dl_stubs.c$(OS_ALT_SUFFIX)
	cp $< $@

src/ctypes/ctypes_primitives.ml: $(BUILDDIR)/configure.native
	$< > $@

src/ctypes-foreign-base/libffi_abi.ml: $(BUILDDIR)/libffi-abigen.native
	$< > $@

setup.data: src/discover/discover.ml
	ocaml $^ -ocamlc "$(OCAMLFIND) ocamlc"

# dependencies
depend: configure
	$(OCAMLDEP) $(foreach project,$(PROJECTS),-I $($(project).dir)) \
            $(shell find src examples -name '*.mli' -o -name '*.ml') \
           | sed "s!src/!_build/src/!g; s!examples/!_build/examples/!g" > .depend

#installation
META-install:
	$(OCAMLFIND) install ctypes META CHANGES.md

install-%: PROJECT=$*
install-%:
	$(if $(filter yes,$($(PROJECT).install)),\
		$(OCAMLFIND) install -add ctypes $^ \
                   $(LIB_TARGETS) $(LIB_TARGET_EXTRAS) \
                   $(INSTALL_MLIS) $(INSTALL_CMIS) \
                   $(INSTALL_HEADERS) \
                   $(if $(filter yes,$($(PROJECT).install_native_objects)),$(NATIVE_OBJECTS)))

install: META-install $(PROJECTS:%=install-%)

uninstall:
	$(OCAMLFIND) remove ctypes

.PHONY: depend distclean clean build configure all install $(PROJECTS)

include .depend Makefile.rules Makefile.examples Makefile.tests
-include setup.data

