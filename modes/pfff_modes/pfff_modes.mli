
(* will modify the text_attrs of the passed buffer and will
 * add an Outline_mode.outline_var buffer variable in the buffer
 *)
val colorize_and_set_outlines: 
  ('ast, 'token) Parse_and_highlight.t -> Efuns.buffer -> Fpath.t -> unit

(* maintain some dir -> db information (useful for goto_def below) *)
val load_database_code: Efuns.action
(* DEAD? used by something? TODO? leverage? *)
val load_graph_code: Efuns.action

val goto_def: Efuns.action

