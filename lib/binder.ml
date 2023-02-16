type 'a t = { name : string; scoped : 'a } [@@deriving show]

let opened bind binder =
  let fresh = Atom.make binder.name in
  (fresh, bind fresh binder.scoped)

let closed unbind atom e =
  { scoped = unbind atom e; name = Atom.name atom }
