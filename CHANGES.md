## ctypes 0.5.1

### Bug fixes

* Use a C function, not `Pervasives.ignore`, to keep values alive.

## ctypes 0.5.0

Thanks to Andreas Hauptmann (@fdopen), David Sheets (@dsheets), Etienne Millon (@emillon), Goswin von Brederlow (@mrvn), Leonid Rozenberg (@rleonid), @orbitz, Max Mouratov (@cakeplus), and Peter Zotov (@whitequark) for contributions to this release.

### Features

* Build and install `*.cmt` and `*.cmti` files.

* Expose `time_t` as an unsigned value

* Expose larger interfaces for POSIX types known to be integer types.

* Add support for 1- and 2-byte unsigned integer typedefs.

* Add support for 1-byte and 2-byte integer typedefs.

* Add a `Signed.Int` module.

* Expose more information in the `Uncoercible` exception.

* `allocate_n` now defaults to zeroing its memory.

* Add public root management interface.

  NB: the interface is experimental and subject to change.

* Look through views to add fields to structs and unions.

* Support signed arithmetic operations for `ssize_t`.

* Add support for `ptrdiff_t` as a typedef for a signed integer type.

* Support `intptr_t` and `uintptr_t` as typedefs

* Support coercions between object and function pointers.

* Add public `funptr_of_raw_address` function.

* Support `static_funptr` coercions

* Add function pointers to the core type language

  (See the `Ctypes_static.static_funptr` type, on which
  `Foreign.funptr` and `Foreign.foreign` are now based.)

* Better support for functions returning void with inverted stubs.

* Add support for releasing runtime lock to Cstubs_inverted.

### Bug fixes

* Fix: inconsistent use of `caml_stat_*` functions

* Fix: a memory leak in `ctypes_caml_roots_release`

## ctypes 0.4.2

* Fix a bug involving access to local roots while the runtime lock was not held.

## ctypes 0.4.1

Thanks to Etienne Millon (@emillon) for contributing to this release.

* Fix placement of docstring titles
* Add funptr's optional arguments to funptr_opt
* Fix a typo in libffi detection code
* Synchronize foreign.mli files (documentation)

## ctypes 0.4

Thanks to A. Hauptmann (@fdopen), David Sheets (@dsheets), Maverick Woo (@maverickwoo), Peter Zotov (@whitequark), David Kaloper (@pqwy), Ramkumar Ramachandra (@artagnon), Thomas Braibant (@braibant), Hugo Heuzard (@hhugo) and Edwin Török (@edwintorok) for contributions to this release.

### Major features

* Support for the C99 bool type

* Typedef support

* Enum support

* Support for accessing C globals with foreign_value in generated stubs

* Support for retrieving #define and enum constants from C

* Windows support

  There is now support for Windows (32-bit and 64-bit, using MinGW) and automatic building and testing on Windows using [Appveyor][appveyor-builds]. 

* Support for releasing the runtime lock in C calls

  The new `release_runtime_lock` argument to `Foreign.foreign` indicates whether the OCaml runtime lock should be released during the call to the bound C function, allowing other threads to run.

* Support for acquiring the runtime lock in callbacks

  There is a  new `runtime_lock` argument to `Foreign.funptr`.  Setting `runtime_lock` to `true` indicates that the OCaml runtime lock should be acquired during calls from C to OCaml and released during calls through function pointers from OCaml to C.

* Support for passing 'bytes' values directly to C

  See the [relevant section of the FAQ][strings_faq].

* Add support for custom printers in views.

* Optionally obtain struct and union layout from C

#### Other changes
* string_opt wraps char *, not void *.
* Remove some poorly-supported POSIX types
* Use nativeint to represent pointers
* Support zero-argument callbacks
* findlib package naming: ctypes.foreign-base ~> ctypes.foreign.base &c.
* Make it possible to print a field name
* Better exception handling when using RTLD_NOLOAD
* RTLD_LOCAL support
* Changed the #include path to $(ocamlfind query ctypes)
* Renamed some internal modules to avoid name clashes

[appveyor-builds]: https://ci.appveyor.com/project/yallop/ocaml-ctypes/branch/master

## ctypes 0.3.4

#### Bug fixes

Thanks to Yakov Zaytsev (@ysz) for contributing to this release.

* fix printing for nullary function stubs

## ctypes 0.3.3

#### Bug fixes

* respect `pbyte_offset` with cstubs

## ctypes 0.3.2

* Add bytes to the META "requires" field

## ctypes 0.3.1

#### New features

* Support for 'bytes'

#### Bug fixes

* Avoid invalidated pointer access

## ctypes 0.3

Thanks to Peter Zotov (@whitequark), David Sheets (@dsheets), Mike McClurg (@mcclurmc) and Anil Madhavapeddy (@avsm) for contributions to this release.

#### Major features

##### Support for passing OCaml strings directly to C
(Patch by Peter Zotov.)

The implications are discussed [in the FAQ][strings_faq].

[strings_faq]: https://github.com/ocamllabs/ocaml-ctypes/wiki/FAQ#strings

##### Support for generating C stubs from names and type declarations.
There are various examples available of packages which use stub support: see the [fts example][fts-example] in the distribution (which uses a custom Makefile), [this fork of async_ssl][async_ssl] (which uses OCamlMakefile), and [the cstubs branch of ocaml-lz4][ocaml-lz4] (which uses oasis and ocamlbuild).

[fts-example]: https://github.com/ocamllabs/ocaml-ctypes/tree/master/examples/fts/stub-generation
[async_ssl]: https://github.com/yallop/async_ssl/tree/stub-generation
[ocaml-lz4]: https://github.com/whitequark/ocaml-lz4/tree/cstubs

##### Support for turning OCaml modules into C libraries.
See the [ocaml-ctypes-inverted-stubs-example][inverted-stubs-example] repository for a sample project which exposes a part of [Xmlm][xmlm]'s API to C.

[inverted-stubs-example]: https://github.com/yallop/ocaml-ctypes-inverted-stubs-example/ 
[xmlm]: http://erratique.ch/software/xmlm

#### Other changes

* Add a function [`string_from_ptr`][string_from_ptr] for creating a string from an address and length.
* Generate [codes for libffi ABI specifications][libffi_abi].
* Support for passing complex numbers to C using the stub generation backend.
* Add [`raw_address_of_ptr`][raw_address_of_ptr], an inverse of [`ptr_of_raw_address`][ptr_of_raw_address].
* Add a function [`typ_of_bigarray_kind`][typ_of_bigarray_kind] for converting `Bigarray.kind` values to `Ctypes.typ` values.
* Improved [coercion][coercion] support

[typ_of_bigarray_kind]: http://ocamllabs.github.io/ocaml-ctypes/Ctypes.html#VALtyp_of_bigarray_kind
[string_from_ptr]: http://ocamllabs.github.io/ocaml-ctypes/Ctypes.html#VALstring_from_ptr
[raw_address_of_ptr]: http://ocamllabs.github.io/ocaml-ctypes/Ctypes.html#VALraw_address_of_ptr
[ptr_of_raw_address]: http://ocamllabs.github.io/ocaml-ctypes/Ctypes.html#VALptr_of_raw_address
[CArray]: http://ocamllabs.github.io/ocaml-ctypes/Ctypes.Array.html
[libffi_abi]: http://ocamllabs.github.io/ocaml-ctypes/Libffi_abi.html
[coercion]: http://ocamllabs.github.io/ocaml-ctypes/Ctypes.html#VALcoerce

#### Backwards incompatibilities

* `Array` has been renamed to [`CArray`][CArray].

## ctypes 0.2.3

#### Bug fixes

* Fix GC-related bug that shows up on OS X.

## ctypes 0.2.2

* Don't install ctypes-foreign cmx files.

## ctypes 0.2.1

* Bump META version

## ctypes 0.2

Thanks to Ivan Gotovchits, Greg Perkins, Daniel Bünzli, Rob Hoes and Anil Madhavapeddy for contributions to this release.

#### Major features

##### Bigarray support.
See [Bigarray types][bigarray-types] and [Bigarray values][bigarray-values] for details.

[bigarray-types]: http://ocamllabs.github.io/ocaml-ctypes/Ctypes.html#4_Bigarraytypes
[bigarray-values]: http://ocamllabs.github.io/ocaml-ctypes/Ctypes.html#4_Bigarrayvalues

##### Give the user control over the lifetime of closures passed to C.
See [the FAQ][faq-lifetime] for details.

[faq-lifetime]: https://github.com/ocamllabs/ocaml-ctypes/wiki/FAQ#function-lifetime

##### Top level printing for C values and types
Loading the new findlib package `ctypes.top` in the toplevel will install custom printers for C types and values.

#### Other changes

* Basic [coercion][coercion] support
* Remove `returning_checking_errno`; pass a flag to [`foreign`][foreign] instead.
* Add an optional argument to [`Foreign.foreign`][foreign] that ignores absent symbols.
  (Patch by Daniel Bünzli.)
* More precise tests for whether types are 'passable'
* Compulsory names for structure and union fields (`*:*` and `+:+` are deprecated, but still supported for now.)
* [`UInt32.of_int32`][of_int32], [`UInt32.to_int32`][to_int32], [`UInt64.of_int64`][of_int64], and [`UInt64.to_int64`][to_int64] functions.
* Finalisers for ctypes-allocated memory.
* Add a [`string_opt`][string_opt] view
  (Patch by Rob Hoes.)
* Add the ['camlint'][camlint] basic type.
* Complex number support
* Abstract types [now have names][abstract].

[foreign]: http://ocamllabs.github.io/ocaml-ctypes/Foreign.html#VALforeign
[of_int32]: http://ocamllabs.github.io/ocaml-ctypes/Unsigned.Uint32.html#VALof_int32
[to_int32]: http://ocamllabs.github.io/ocaml-ctypes/Unsigned.Uint32.html#VALto_int32
[of_int64]: http://ocamllabs.github.io/ocaml-ctypes/Unsigned.Uint64.html#VALof_int64
[to_int64]: http://ocamllabs.github.io/ocaml-ctypes/Unsigned.Uint64.html#VALto_int64
[string_opt]: http://ocamllabs.github.io/ocaml-ctypes/Ctypes.html#VALstring_opt
[camlint]: http://ocamllabs.github.io/ocaml-ctypes/Ctypes.html#VALcamlint
[abstract]: http://ocamllabs.github.io/ocaml-ctypes/Ctypes.html#VALabstract
[coercion]: http://ocamllabs.github.io/ocaml-ctypes/Ctypes.html#VALcoerce

## ctypes 0.1.1

#### Bug fixes

* Remove hard-coded alloc size

## ctypes 0.1

initial release
