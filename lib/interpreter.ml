open Syntax;;


type value =
    Int of int
  | Lam of (value -> value)
;;


module Ctx = struct

  type t = {
    scope: (ident * value) list;
  }

  let empty () = { scope = [] }

  let ident ctx ident = List.assoc ident ctx.scope;;
  let ident_opt ctx ident = List.assoc_opt ident ctx.scope;;

  let add ctx ident value = { scope = (ident,value)::ctx.scope }
  let pop ctx ident = { scope = List.remove_assoc ident ctx.scope }

end;;

exception TypeError of value;;

let unwrap_int expr = match expr with
  | Int i -> i
  | _ -> raise (TypeError expr)

let lam_of_binop f = Lam (
    fun a -> let a = unwrap_int a in
    Lam (fun b -> let b = unwrap_int b in
       Int(f a b))
)

let base_ctx =
  Ctx.({ scope = [
      "x", Int 1;
      "y", Int 2;
      "z", Int 3;
      "+", lam_of_binop ( + );
      "-", lam_of_binop ( - );
      "*", lam_of_binop ( * );
      "/", lam_of_binop ( / );
  ]})



let rec interpret ctx expr = match expr with
  | EIdent id -> Ctx.ident ctx id
  | EFun (id,body) -> Lam(fun arg ->
      interpret (Ctx.add ctx id arg) body)
  | EApp(f, x) -> let f = interpret ctx f in match f with
    | Lam f -> f (interpret ctx x)
    | _ -> raise (TypeError f)
