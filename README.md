ctypes is a library for binding to C libraries using pure OCaml.  The primary aim is to make writing C extensions as straightforward as possible.

The core of ctypes is a set of combinators for describing the structure of C types -- numeric types, arrays, pointers, structs, unions and functions.  You can use these combinators to describe the types of the functions that you want to call, then bind directly to those functions -- all without writing or generating any C!

For example, suppose you want to bind to the following C functions:

```C
   int sigemptyset(sigset_t *set);
   int sigfillset(sigset_t *set);
   int sigaddset(sigset_t *set, int signum);
   int sigdelset(sigset_t *set, int signum);
   int sigismember(const sigset_t *set, int signum);
```

Using ctypes you can describe the interfaces to these functions as follows:

```OCaml
   let sigemptyset = foreign "sigemptyset" (ptr sigset_t @-> returning int)
   let sigfillset = foreign "sigfillset" (ptr sigset_t @-> returning int)
   let sigaddset = foreign "sigaddset" (ptr sigset_t @-> int @-> returning int)
   let sigdelset = foreign "sigdelset" (ptr sigset_t @-> int @-> returning int)
   let sigismember = foreign "sigismember" (ptr sigset_t @-> int @-> returning int)
```

The names bound by this code have the types you might expect:

```OCaml
   val sigemptyset : sigset_t ptr -> int
   val sigfillset : sigset_t ptr -> int
   val sigaddset : sigset_t ptr -> int -> int
   val sigdelset : sigset_t ptr -> int -> int
   val sigismember : sigset_t ptr -> int -> int
```

That's all there is to it.  Unlike the [usual way](http://caml.inria.fr/pub/docs/manual-ocaml-4.00/manual033.html) of writing C extensions, there are no C "stub" functions to write, so there's much less opportunity for error.

The documentation and source distribution contain more complex examples, involving structs, unions, arrays, callback functions, and so on, and show how to create and use C values (like instances of `sigset_t ptr`) in OCaml.

## Links

* [Tutorial](https://github.com/ocamllabs/ocaml-ctypes/wiki/ctypes-tutorial)
* [API documentation](http://ocamllabs.github.io/ocaml-ctypes)
* [Mailing list](http://lists.ocaml.org/listinfo/ctypes)
