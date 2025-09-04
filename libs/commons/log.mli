(* DEPRECATED: should use the Logs library instead *)

val printf: ('a -> 'b, out_channel, unit) format -> 'a -> unit

val exn: (string -> 'a, out_channel, unit) format -> exn -> unit
