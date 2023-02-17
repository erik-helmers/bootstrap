(** Génération de noms uniques *)

type t = { uid : int; name : string }

let pp fmt a = Format.fprintf fmt "%s@%d" a.name a.uid

let global_reset, global_nextuid =
  let count = ref 0 in
  ( (fun () -> count := 0),
    fun () ->
      incr count;
      !count )

let make name = { name; uid = global_nextuid () }

let cmp a1 a2 =
  assert (a1.uid <> a2.uid || a1.name = a2.name);
  Stdlib.compare a1.uid a2.uid

let eq a1 a2 = cmp a1 a2 = 0
let name a = a.name

(* Only used by pprint *)
let uid a = a.uid
