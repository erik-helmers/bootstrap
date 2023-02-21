(** Génération de noms uniques *)

type t = { uid : int; name : string }

let pp fmt a = Format.fprintf fmt "%s" a.name
let pp_dump fmt a = Format.fprintf fmt "%s@%d" a.name a.uid

let make =
  let next = ref 0 in
  fun name ->
    incr next;
    { name; uid = !next }

let cmp a1 a2 =
  assert (a1.uid <> a2.uid || a1.name = a2.name);
  Stdlib.compare a1.uid a2.uid

let eq a1 a2 = cmp a1 a2 = 0
let name a = a.name

(* Only used by pprint *)
let uid a = a.uid
