type t

val make : string -> t
val make_nameless : unit -> t
val cmp : t -> t -> int
val eq : t -> t -> bool
val name : t -> string
val global_reset : unit -> unit
val pp : Format.formatter -> t -> unit

module Quote : sig
  val make : int -> t
  val is_quote : t -> bool
  val depth : t -> int
end
