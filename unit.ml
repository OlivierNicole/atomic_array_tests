open QCheck

module type TypeSpec = sig
  type t
  val name : string
  val gen : t Gen.t
  val print : t Print.t
end
(** In addition, [t] must support built-in equality. *)

module TestAtomic (T : TypeSpec) = struct
  let get_spec (a,args) =
    args |> List.for_all (fun index ->
      a.(index) = Atomic.Array.get a index
    )

  let set_spec (a,args) =
    args |> List.for_all (fun (index,v) ->
      Atomic.Array.set a index v;
      a.(index) = v
    )

  let exchange_spec (a,args) =
    args |> List.for_all (fun (index,v) ->
      let old = a.(index) in
      let old' = Atomic.Array.exchange a index v in
      old = old' && a.(index) = v
    )

  (* Takes an array and a list of (index,oldval,newval) triples and verifies that
     [compare_and_set] behaves as expected. *)
  let compare_and_set_spec (a,args) =
    args |> List.for_all (fun (index,(oldval,newval)) ->
      let old = a.(index) in
      let res = Atomic.Array.compare_and_set a index oldval newval in
      old <> oldval || (res && a.(index) = newval)
    )

  let gen_get_args =
    let open Gen in
    let* a = array T.gen in
    let length = Array.length a in
    let* args = if length <> 0 then list (int_bound (length-1)) else return [] in
    return (a,args)

  let print_get_args = Print.(pair (array T.print) (list int))

  let get_args = make gen_get_args ~print:print_get_args

  let gen_set_args =
    let open Gen in
    let* a = array T.gen in
    let length = Array.length a in
    let* args = if length <> 0 then list (pair (int_bound (length-1)) T.gen) else return [] in
    return (a,args)

  let print_set_args = Print.(pair (array T.print) (list (pair int T.print)))

  let set_args = make gen_set_args ~print:print_set_args

  (* Generate a pair [(x,x')] with a 1/2 probability that [x = x']. *)
  let mypair =
    let open Gen in
    let* x = T.gen in
    let* b = bool in
    if b then return (x,x) else pair (return x) T.gen

  let gen_cas_args =
    let open Gen in
    let* a = array T.gen in
    let length = Array.length a in
    let* args =
      if length <> 0 then
        list (pair (int_bound (length-1)) mypair)
      else
        return []
    in
    return (a,args)
  ;;

  let print_cas_args =
    Print.(pair (array T.print) (list (pair int (pair T.print T.print))))

  let cas_args = make gen_cas_args ~print:print_cas_args

  let tests = [
    Test.make ~name:(T.name^" array: atomic get") ~count:1000 get_args get_spec;
    Test.make ~name:(T.name^" array: atomic set") ~count:1000 set_args set_spec;
    Test.make ~name:(T.name^" array: atomic exchange") ~count:1000 set_args set_spec;
    Test.make ~name:(T.name^" array: atomic compare_and_set") ~count:1000 cas_args compare_and_set_spec;
  ]
end

(* The specification of [fetch_add] is not parameterized by a type as it is
   only defined for int arrays. *)
(* Takes an array and a list of (index,incr) pairs and verifies that
   [fetch_and_add] behaves as expected. *)
let fetch_add_spec (a,args) =
  args |> List.for_all (fun (index,incr) ->
    let old = a.(index) in
    let old' = Atomic.Array.fetch_and_add a index incr in
    old = old' && a.(index) = old + incr
  )

(* Generates an array and a list of (index,incr) pairs *)
let gen_fetch_add_args =
  let open Gen in
  let* a = array int in
  let length = Array.length a in
  let* args = if length <> 0 then list (pair (int_bound (length-1)) int) else return [] in
  return (a,args)

let print_fetch_add_args =
  Print.(pair (array int) (list (pair int int)))

module Int : TypeSpec = struct
  type t = int
  let name = "int"
  let gen = Gen.int
  let print = Print.int
end

module Float : TypeSpec = struct
  type t = float
  let name = "float"
  let gen = Gen.float
  let print = Print.float
end

module IntFPairOpt : TypeSpec = struct
  type t = (int * float) option
  let name = "(int*float) option"
  let gen = Gen.(opt (pair int float))
  let print = Print.(option (pair int float))
end

module IntAtomicTests = TestAtomic(Int)
module FloatAtomicTests = TestAtomic(Float)
module IntFPairOptAtomicTests = TestAtomic(IntFPairOpt)

let fetch_add_args = make gen_fetch_add_args ~print:print_fetch_add_args

let () = QCheck_runner.run_tests_main @@
  IntAtomicTests.tests @
  FloatAtomicTests.tests @
  IntFPairOptAtomicTests.tests @
  [ Test.make ~name:"atomic fetch_and_add" ~count:1000 fetch_add_args fetch_add_spec;
  ]
