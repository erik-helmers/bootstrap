type 'a t = { name : string; scoped : 'a }

let pp pp_a fmt t =
  let open Format in
  fprintf fmt "@[%s@ ->@ %a@]" t.name pp_a t.scoped

let open_ bind t =
  let fresh = Atom.make t.name in
  (fresh, bind fresh t.scoped)

let close_ unbind atom expr =
  { name = Atom.name atom; scoped = unbind atom expr }

let subst bind t v = bind v t.scoped
let map f b = { b with scoped = f b.scoped }

(* same as map, but you do not have to remember to increment *)
let weaken f i b = { b with scoped = f (i + 1) b.scoped }
