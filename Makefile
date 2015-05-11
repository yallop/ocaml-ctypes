.SECONDEXPANSION:

DEBUG=false
COVERAGE=false
OCAML=ocaml
OCAMLFIND=ocamlfind
OCAMLDEP=$(OCAMLFIND) ocamldep
OCAMLMKLIB=$(OCAMLFIND) ocamlmklib
VPATH=src examples
BUILDDIR=_build
BASE_PROJECTS=configure libffi-abigen configured ctypes ctypes-top
FOREIGN_PROJECTS=test-libffi ctypes-foreign-base ctypes-foreign-threaded ctypes-foreign-unthreaded
STUB_PROJECTS=cstubs
PROJECTS=$(BASE_PROJECTS) $(FOREIGN_PROJECTS) $(STUB_PROJECTS)
GENERATED=src/ctypes/ctypes_primitives.ml	\
          src/ctypes-foreign-base/libffi_abi.ml \
          src/ctypes-foreign-base/dl.ml		\
          src/ctypes-foreign-base/dl_stubs.c	\
          libffi.config				\
          asneeded.config 
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
all: libffi.config $(PROJECTS)

ctypes-base: $(BASE_PROJECTS)
ctypes-foreign: ctypes-base $(FOREIGN_PROJECTS)
ctypes-stubs: ctypes-base $(STUB_PROJECTS)

clean:
	rm -fr _build
	rm -f $(GENERATED)

# ctypes subproject
ctypes.public = ctypes_static ctypes_primitive_types unsigned signed ctypes_structs ctypes posixTypes ctypes_types
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
cstubs.public = cstubs_internals cstubs_structs cstubs cstubs_inverted
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

libffi.config: src/discover/commands.mli src/discover/commands.ml src/discover/discover.ml
	@ocamlfind ocamlc -o discover -package str,bytes -linkpkg $^ -I src/discover
	./discover -ocamlc "$(OCAMLFIND) ocamlc" > $@ || (rm $@ && false)

asneeded.config:
	./src/discover/determine_as_needed_flags.sh >> $@

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
		$(OCAMLFIND) install -add ctypes -optional $^ \
                   $(LIB_TARGETS) $(LIB_TARGET_EXTRAS) \
                   $(INSTALL_MLIS) $(INSTALL_CMIS) \
                   $(INSTALL_HEADERS) \
                   $(if $(filter yes,$($(PROJECT).install_native_objects)),$(NATIVE_OBJECTS)))

install: META-install $(PROJECTS:%=install-%)

uninstall:
	$(OCAMLFIND) remove ctypes

DOCFILES=$(foreach project,$(PROJECTS),\
           $(foreach mli,$($(project).public),\
            $($(project).dir)/$(mli).mli))
DOCFLAGS=$(foreach project,$(PROJECTS),-I $(BUILDDIR)/$($(project).dir))
# Avoid passing duplicate interfaces to ocamldoc.
DOCFILES:=$(filter-out src/ctypes-foreign-threaded/foreign.mli,$(DOCFILES))

doc:
	ocamldoc -html $(DOCFLAGS) $(DOCFILES)


.PHONY: depend clean configure all install doc $(PROJECTS)

include .depend Makefile.rules Makefile.examples Makefile.tests
-include libffi.config
-include asneeded.config

ifeq ($(libffi_available),false)
test-libffi:
	@echo "The following required C libraries are missing: libffi."
	@echo "Please install them and retry. If they are installed in a non-standard location"
	@echo "or need special flags, set the environment variables <LIB>_CFLAGS and <LIB>_LIBS"
	@echo "accordingly and retry."
	@echo
	@echo " For example, if libffi is installed in /opt/local, you can type:"
	@echo
	@echo "   export LIBFFI_CFLAGS=-I/opt/local/include"
	@echo "   export LIBFFI_LIBS=-L/opt/local/lib"
	@exit 1
else:
test-libffi:
endif
