open Syntax;;


type info =
    HasType of ty
  | HasKind of kind

type name = string;;

type t = {
    types: (name * info) list;
    values: (name * value) list;
};;

let empty () = { types = []; values = [] }



let unwrap_int expr = match expr with
  | Int i -> i
  | _ -> failwith "typing: expected int"

let lam_of_binop f = Lam (
    fun a -> let a = unwrap_int a in
    Lam (fun b -> let b = unwrap_int b in
       Int(f a b))
)


let ty_of_binop = TFun(Ty "int", TFun (Ty "int", Ty "int"))
let base_ctx ={ values = [
      "x", Int 1;
      "y", Int 2;
      "z", Int 3;
      "+", lam_of_binop ( + );
      "-", lam_of_binop ( - );
      "*", lam_of_binop ( * );
      "/", lam_of_binop ( / );
  ];  types = [
      "int", HasKind Star;
      "x", HasType (Ty "int");
      "y", HasType (Ty "int");
      "z", HasType (Ty "int");
      "+", HasType ty_of_binop;
      "-", HasType ty_of_binop;
      "*", HasType ty_of_binop;
      "/", HasType ty_of_binop;
  ]}

let ident_ty ctx ident = List.assoc ident ctx.types;;
let ident_ty_opt ctx ident = List.assoc_opt ident ctx.types;;

let add_ident_ty ctx ident ty = { ctx with types = (ident,ty)::ctx.types }
let pop_ident_ty ctx ident = { ctx with types = List.remove_assoc ident ctx.types }

let ident_val ctx ident = List.assoc ident ctx.values;;
let ident_val_opt ctx ident = List.assoc_opt ident ctx.values;;

let add_ident_val ctx ident value = { ctx with values = (ident,value)::ctx.values }
let pop_ident_val ctx ident = { ctx with values = List.remove_assoc ident ctx.values }
