type 'a scoped = Scoped of 'a

let pp_scoped pp_a fmt (Scoped s) =
  let open Format in
  fprintf fmt "@[%a@]" pp_a s

let scoped t = Scoped t

type 'a t = { name : string; scoped : 'a scoped }

let pp pp_a fmt t =
  let open Format in
  fprintf fmt "@[%s@ ->@ %a@]" t.name pp_a t.scoped

let open_ bind t =
  let fresh = Atom.make t.name in
  (fresh, bind fresh t.scoped)

let close unbind atom expr =
  { name = Atom.name atom; scoped = unbind atom expr }

let subst bind t v = bind v t.scoped

(** Here be dragons *)
(* The following are unsafe functions that will *)
(* make your world crumble  *)

let unscoped (Scoped v) = v
let unwrap b = unscoped b.scoped

(*  very unsafe  *)
let map f b = { b with scoped = Scoped (f @@ unscoped b.scoped) }

(* is the name right ? *)
(* same as map, but you do not have to remember to increment *)
let weaken f i b =
  { b with scoped = Scoped (f (i + 1) @@ unscoped b.scoped) }
