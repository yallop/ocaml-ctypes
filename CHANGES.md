== ctypes 0.3.4 ==

03da4b: fix printing for nullary function stubs

== ctypes 0.3.3 ==

2e3b3a: respect `pbyte_offset` with cstubs

== ctypes 0.3.2 ==

2a651f: Add bytes to the META "requires" field

== ctypes 0.3.1 ==

3d6ea0: Support for 'bytes'
518335: Avoid invalidated pointer access

== ctypes 0.3 ==

4e6d65: Support for exposing OCaml code as C libraries
3d3747: Support passing OCaml strings directly to C
6e87ed: Support for stub generation 
b01009: Support for selecting libffi ABIs
b09d74: Rename Array to 'CArray' and array to 'carray'
e4b51e: Add raw_address_of_ptr
a759ca: Add typ_of_bigarray_kind
9e1065: Mention coercion transitivity.
405d5a: Clarify when copying occurs.
c83532: Add debug build support
014f1c: Fix raw pointer conversions arithmetic
4050fa: Make RTLD_NODELETE/RTLD_NOLOAD (unavailable on OpenBSD) optional
381982: Add string_from_ptr
4cef4e: Add ctypes_copy_bytes
68e568: Print bigarray types using the C type syntax
372d0f: Handle value printing where the result contains null characters
061128: Handle non-array values masquerading as arrays
a8f639: Catch attempts to treat unstructured types as structured types
dab586: Function coercions
dd1058: Eliminate the cost of identity coercions
7d90de: Include C objects in the native plugins
99c576: Improved --no-as-needed support
579766: Fix Makefile typo for install_native_objects
2a0cbc: Allow coercions between identical primitive types
399833: Switch bigarray_of_array/array_of_bigarray docstrings

== ctypes 0.2.3 ==

39cacd: Fix GC-related bug that shows up on OS X.

== ctypes 0.2.2 ==

608714: Don't install ctypes-foreign cmx files.

== ctypes 0.2.1 ==

d5389f: Bump META version

== ctypes 0.2 ==

c8d54a: Basic coercion support
2552d3: Build fix for Debian Squeeze
d1131d: Bigarray support
816b56: Replace returning_checking_errno with an option to Foreign.foreign
1a8ead: Check homebrew prefix during configuration
554939: Foreign.foreign, give the choice of not failing if symbol is absent.
038420: Tweak passability tests to distinguish libffi's restrictions from C's.
df10e1: Move funptr and funptr_opt to the Foreign module.
71a4c7: Struct type equality is now structural, not physical
510b7a: Compulsory field names (deprecate *:* and +:+)
349422: Fix bug on 32-bit platforms
7d0037: More efficient string view implementation.
8c5b93: Expose field type function.
2d38bc: UInt32.(of|to)_int32 and UInt64.(of|to)_int64 functions.
ea135b: Eliminate build-time oasis dependency
d6fc68: Finalisers for ctypes-allocated memory.
ea6ef2: Expose the camlint type in the public interface.
7a7106: Add the 'camlint' basic type.
9caf55: Treat sealing empty structs or unions as an error.
84ea43: Support for C99's complex number types.
e2a42e: Add string_opt view
8ae5a3: Give the user control over the lifetime of closures passed to C.
db1ee3: Findlib target for installing top-level pretty printers.
378a02: Pretty-printing for C values.
ee178a: Pretty-printing for C types.
4ce9b5: Abstract types now have names

== ctypes 0.1.1 ==

28c06: Bugfix (hard-coded alloc size)

== ctypes 0.1 ==

initial release
