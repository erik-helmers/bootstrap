type t = { uid : int; name : string } [@@deriving show]

(** We want this to stay unique *)
let global_reset, global_nextuid =
  let count = ref 0 in
  ( (fun () -> count := 0),
    fun () ->
      incr count;
      !count )

let make name = { name; uid = global_nextuid () }

let make_nameless () =
  make (Printf.sprintf "<nameless %d>" (global_nextuid ()))

let cmp a1 a2 =
  assert (a1.uid <> a2.uid || a1.name = a2.name);
  Stdlib.compare a1.uid a2.uid

let eq a1 a2 = cmp a1 a2 = 0
let name a = a.name

module Quote = struct
  let make i = { name = Printf.sprintf "<quote %d>" i; uid = -i - 1 }
  let is_quote a = a.uid < 0
  let depth a = -a.uid - 1
end
