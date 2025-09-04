
val add_reader : Unix.file_descr -> (unit -> unit) -> unit 
val remove_reader : Unix.file_descr -> unit

(*
not used anymore inside efuns:

val iterator : int ref -> unit

val poll : unit -> bool

val add_timer : float -> (unit -> unit) -> unit

val fork : unit -> int
*)
