open Ffi.C
open Struct
open OUnit

module Hierarchy :
sig
  type point2d
  type point3d = private point2d
  type coloured_point2d = private point2d

  val point2d : point2d structure typ
  val point3d : point3d structure typ
  val coloured_point2d : coloured_point2d structure typ

  val make_point2d : _x:int -> _y:int -> point2d structure
  val make_point3d : _x:int -> _y:int -> _z:int -> point3d structure
  val make_coloured_point2d : _x:int -> _y:int -> _colour:char ->
    coloured_point2d structure

  val x : (int, point2d) field
  val y : (int, point2d) field
  val z : (int, point3d) field
  val colour : (char, coloured_point2d) field
end =
struct
  open Type

  type point2d

  let point2d = tag "point2d"
  let x = point2d *:* int
  let y = point2d *:* int
  let () = seal point2d

  let make_point2d ~_x ~_y =
    let p2 = Struct.make point2d in
    let () = setf p2 x _x in
    let () = setf p2 y _y in
    p2

  type point3d = private point2d

  let point3d : point3d structure typ = tag "point3d"
  let base = point3d *:* point2d
  let z = point3d *:* int
  let () = seal point3d

  let make_point3d ~_x ~_y ~_z =
    let p3 : point3d structure = Struct.make point3d in
    let base = (p3 :> point2d structure) in
    let () = setf base x _x in
    let () = setf base y _y in
    let () = setf p3 z _z in
    p3

  type coloured_point2d = private point2d

  let coloured_point2d : coloured_point2d structure typ
      = tag "coloured_point2d"
  let base = coloured_point2d *:* point2d
  let colour = coloured_point2d *:* char
  let () = seal coloured_point2d

  let make_coloured_point2d ~_x ~_y ~_colour =
    let cp : coloured_point2d structure = Struct.make coloured_point2d in
    let base = (cp :> point2d structure) in
    let () = setf base x _x in
    let () = setf base y _y in
    let () = setf cp colour _colour in
    cp
end

let main =
  let open Hierarchy in
  let p2 = make_point2d ~_x:10 ~_y:20 in
  let p3 = make_point3d ~_x:100 ~_y:200 ~_z:300 in
  let cp = make_coloured_point2d ~_x:1000 ~_y:2000 ~_colour:'r' in
  
  (* structure subtyping *)
  assert_equal (getf p2 x) 10
    ~printer:string_of_int;

  assert_equal (getf p2 y) 20
    ~printer:string_of_int;

  assert_equal (getf (p3 :> point2d structure) x) 100
    ~printer:string_of_int;

  assert_equal (getf (p3 :> point2d structure) y) 200
    ~printer:string_of_int;

  assert_equal (getf p3 z) 300
    ~printer:string_of_int;

  assert_equal (getf (cp :> point2d structure) x) 1000
    ~printer:string_of_int;

  assert_equal (getf (cp :> point2d structure) y) 2000
    ~printer:string_of_int;

  assert_equal (getf cp colour) 'r';

  setf p2 x 11;
  setf p2 y 21;

  setf (p3 :> point2d structure) x 101;
  setf (p3 :> point2d structure) y 201;
  setf p3 z 301;

  setf (cp :> point2d structure) x 1001;
  setf (cp :> point2d structure) y 2001;
  setf cp colour 'b';
  
  assert_equal (getf p2 x) 11
    ~printer:string_of_int;

  assert_equal (getf p2 y) 21
    ~printer:string_of_int;

  assert_equal (getf (p3 :> point2d structure) x) 101
    ~printer:string_of_int;

  assert_equal (getf (p3 :> point2d structure) y) 201
    ~printer:string_of_int;

  assert_equal (getf p3 z) 301
    ~printer:string_of_int;

  assert_equal (getf (cp :> point2d structure) x) 1001
    ~printer:string_of_int;

  assert_equal (getf (cp :> point2d structure) y) 2001
    ~printer:string_of_int;

  assert_equal (getf cp colour) 'b';

  (* field subtyping *)
  setf p2 x 12;
  setf p2 y 22;

  setf p3 (x :> (int, point3d) field) 102;
  setf p3 (y :> (int, point3d) field) 202;
  setf p3 z 302;

  setf cp (x :> (int, coloured_point2d) field) 1002;
  setf cp (y :> (int, coloured_point2d) field) 2002;
  setf cp colour 'y';
  
  assert_equal (getf p2 x) 12
    ~printer:string_of_int;

  assert_equal (getf p2 y) 22
    ~printer:string_of_int;

  assert_equal (getf p3 (x :> (int, point3d) field)) 102
    ~printer:string_of_int;

  assert_equal (getf p3 (y :> (int, point3d) field)) 202
    ~printer:string_of_int;

  assert_equal (getf p3 z) 302
    ~printer:string_of_int;

  assert_equal (getf cp (x :> (int, coloured_point2d) field)) 1002
    ~printer:string_of_int;

  assert_equal (getf cp (y :> (int, coloured_point2d) field)) 2002
    ~printer:string_of_int;

  assert_equal (getf cp colour) 'y';

  print_endline "ok" 
