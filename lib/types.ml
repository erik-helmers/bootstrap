type atom = Atom.t
and 'a binder = 'a Binder.t

type term =
  | Free of atom
  | Bound of int
  | Bool of bool
  | Lam of term binder
  | Pi of term * term binder
  | Sigma of term * term binder

module Binding : sig
  val open_ : term binder -> atom * term
  val close : atom -> term -> term binder
  val bind : term binder -> term -> term
end = struct
  let unsafe_subst map term =
    let rec aux i term =
      match map i term with
      | Some v -> v
      | None -> (
          match term with
          | Free _ | Bound _ | Bool _ -> term
          | Lam f -> Lam (Binder.weaken aux i f)
          | Pi (t, f) -> Pi (t, Binder.weaken aux i f)
          | Sigma (t, f) -> Sigma (t, Binder.weaken aux i f))
    in
    aux 0 term

  (* Substitues Bound 0 for v in s  *)
  let scoped_bind v s =
    unsafe_subst
      (fun i t -> if t = Bound i then Some v else None)
      s

  (* Substitues Free a for Bound 0 in s  *)
  let scoped_unbind a t =
    unsafe_subst
      (fun i t -> if t = Free a then Some (Bound i) else None)
      t

  let open_ = Binder.open_ (fun a -> Free a |> scoped_bind)
  let close = Binder.close scoped_unbind
  let bind = Binder.subst scoped_bind
end

include Binding
