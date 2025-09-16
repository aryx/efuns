(***********************************************************************)
(*                                                                     *)
(*                             ____                                    *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

val count_char_sub : string -> int -> int -> char -> int * int
val count_char : string -> char -> int * int

val list_nth : int -> 'a list -> 'a
val mem_assq : 'a -> ('a * 'b) list -> bool
val removeq : 'a -> ('a * 'b) list -> ('a * 'b) list
val list_removeq : 'a list -> 'a -> 'a list
val remove_assocq : 'a -> ('a * 'b) list -> ('a * 'b) list
val list_remove : 'a list -> 'a -> 'a list
val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list

val file_list : string -> string list
val string_ncmp : string -> string -> int -> bool
val completion : string list -> string -> string list
val common_suffix : string list -> string -> string * int
val read_string : in_channel -> string

val list_of_hash : ('a, 'b) Hashtbl.t -> ('a * 'b) list
val find_in_path : string list -> string -> string

val string_to_path : string -> string list
val path_to_string : string list -> string
val hash_add_assoc : ('a, 'b) Hashtbl.t -> ('a * 'b) list -> unit

val homedir : string
val known_dirs : (string * string) list ref

val check_prefix : string -> string -> string
val replace_prefix : string -> (string * string) list -> string

val filename_to_string : string -> string
val string_to_filename : string -> string

val exns : (exn -> string) list ref
val register_exn : (exn -> string) -> unit
val printexn : exn -> string
val catchexn : string -> (unit -> unit) -> unit
val vcatchexn : string -> (unit -> 'a) -> 'a option

(*val format_to_string : ('a -> 'b) -> 'a -> string*)
val do_and_format : ('a -> 'b) -> 'a -> string * 'b
val format_to_string : ('a -> 'b) -> 'a -> string

val is_directory : string -> bool
val is_link : string -> bool

val list_dir : string -> string list
val load_directory : string -> string
val normal_name : string -> string -> string
val to_regexp_string : string -> string

val hashtbl_mem : ('a, 'b) Hashtbl.t -> 'a -> bool
(*: val glob_to_regexp : string -> string *)
val list_dir_normal : string -> string list
