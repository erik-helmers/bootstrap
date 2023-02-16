open Types

type info = value

type t = {
  (* The type checker only considers free variable *)
  types : (atom * info) list;
  free : (atom * value) list;
  bound : value list;
}

let empty = { types = []; free = []; bound = [] }
let base_ctx = { free = []; types = []; bound = [] }

(* Bound variables *)
let push ctx value = { ctx with bound = value :: ctx.bound }
let bound ctx i = List.nth ctx.bound i

(* Free variables *)
let name_ty ctx name =
  try List.assoc name ctx.types
  with Not_found -> failwith "context: missing name ty"

let name_ty_opt ctx name = List.assoc_opt name ctx.types
let add_name_ty ctx name ty = { ctx with types = (name, ty) :: ctx.types }

let pop_name_ty ctx name =
  { ctx with types = List.remove_assoc name ctx.types }

let name_val ctx name =
  try List.assoc name ctx.free
  with Not_found -> failwith "context: missing name val "

let name_val_opt ctx name = List.assoc_opt name ctx.free

let add_name_val ctx name value =
  { ctx with free = (name, value) :: ctx.free }

let pop_name_val ctx name =
  { ctx with free = List.remove_assoc name ctx.free }

let add_name ctx name value ty =
  let ctx = add_name_val ctx name value in
  let ctx = add_name_ty ctx name ty in
  ctx

let pop_name ctx name =
  let ctx = pop_name_val ctx name in
  let ctx = pop_name_ty ctx name in
  ctx
