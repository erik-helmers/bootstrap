type t = { id : int; name : string } [@@deriving show]

(** We want this to stay unique *)
let global_reset, global_next_id =
  let count = ref 0 in
  ( (fun () -> count := 0),
    fun () ->
      incr count;
      !count )

let make name = { name; id = global_next_id () }

let make_nameless () =
  make (Printf.sprintf "<nameless %d>" (global_next_id ()))

let cmp a1 a2 =
  assert (a1.id <> a2.id || a1.name = a2.name);
  Stdlib.compare a1.id a2.id

let eq a1 a2 = cmp a1 a2 = 0
let name a = a.name
