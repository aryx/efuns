(***********************************************************************)
(*                                                                     *)
(*                             GwML                                    *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
module S = Stream
[@@alert "-all"] (* Stream is deprecated *)

module G = Genlex_
[@@alert "-all"] (* Stream is deprecated *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Configuration file loader and saver with typed options.
 * 
 * alt:
 *  - a JSON file, but this still requires the mechanism of define_type
 *    to have typed options (without the need to pattern Match Json_type.t
 *    to unbox the real type)
 *    => use ATD! and have typed JSON.
 *  - a sexp file
 *  - YAML, TOML, ...
 *)

(*****************************************************************************)
(* Types and globals *)
(*****************************************************************************)
  
type value =
  Module of module_
| Value of  string
| List of value list
| SmallList of value list

and module_ =
  (string * value) list


(* old: was called option_class before (hence some field names) *)
type 'a type_ = {
    class_name : string;
    from_value : value -> 'a;
    to_value : 'a -> value;
    mutable class_hooks : ('a t -> unit) list;
  }

(* old: was called option_record before *)
and 'a t = {
    option_name : string list;
    option_class : 'a type_;
    mutable option_value : 'a;
    option_help : string;
    mutable option_hooks : (unit -> unit) list;
  }

(* alt: could be a parameter of init and save *)
let filename = ref (Filename.concat Utils.homedir
      ("." ^ (Filename.basename Sys.argv.(0)) ^ "rc"))

(* old: there was also a gwmlrc global similar to options so that
 * one could store options in a .efunsrc and .gwmlrc file.
 *)
let options = ref []

let to_value cl = cl.to_value
let from_value cl = cl.from_value

(*****************************************************************************)
(* define_type *)
(*****************************************************************************)

let define_type
  (class_name : string)
  (from_value : value -> 'a)
  (to_value : 'a -> value) =
  {
      class_name = class_name;
      from_value = from_value;
      to_value = to_value;
      class_hooks = [];
  }

(*****************************************************************************)
(* define_option *)
(*****************************************************************************)
  
let rec find_value list m =
  match list with
    [] -> raise Not_found
  | name :: tail ->
      let m = List.assoc name m in
      match m, tail with
        _, [] -> m
      | Module m, _ :: _ -> find_value tail m
      | _ -> raise Not_found
  
let define_option 
  (option_name : string list)
  (option_help : string)
  (option_class : 'a type_)
  (default_value : 'a)
  =
  let o = {
      option_name = option_name;
      option_help = option_help;
      option_class = option_class;
      option_value = default_value;
      option_hooks = [];
    } in

  (* less: could check if already defined option with 
   * find_value option_name !options
   *)    
  options := (Obj.magic o : Obj.t t) :: !options;
  o

(*****************************************************************************)
(* Parser *)
(*****************************************************************************)
  
let lexer = 
  G.make_lexer [ "=" ; "{" ; "}"; "["; "]"; ";" ; "("; ")"; ","; "."]

open G

let rec parse stream = 
  match S.peek stream with
  | None -> []
  | Some t ->
    (match t with
    (* first(id) *)
    | Ident _ | String _ -> 
      let id = parse_id stream in
      (match S.next stream with
      | (Kwd "=") -> ()
      | _ -> raise S.Failure
      );
      let v = parse_option stream in
      (id, v)::parse stream
    | _ -> []
    )

and parse_id stream = 
  match S.next stream with
  | Ident s -> s
  | String s -> s
  | _ -> raise S.Failure

and parse_option stream =
  match S.next stream with
  | Ident s -> Value s
  | String s -> Value s
  | Int i -> Value (string_of_int i)
  | Float f -> Value (string_of_float f)
  | Char c -> Value (String.make 1 c)
  | Kwd "{" ->
    let xs = parse stream in
    (match S.next stream with
    | Kwd "}" -> Module xs
    | _ -> raise S.Failure
    )
  | Kwd "[" | Kwd "(" -> 
    let xs = parse_list stream in
    List xs
  | _ -> raise S.Failure

and parse_list stream =
  match S.peek stream with
  | Some (Kwd "]" | Kwd ")") -> 
    S.junk stream; 
    []
  | Some (Kwd ";" | Kwd "," | Kwd ".") ->
    S.junk stream;
    parse_list stream
  | _ -> 
    let v = parse_option stream in
    let xs = parse_list stream in
    v::xs

(*****************************************************************************)
(* Loading *)
(*****************************************************************************)

let exec_hooks o =
  List.iter (fun f -> try f () with _ -> ()) o.option_hooks  

let exec_chooks o =
  List.iter (fun f -> try f o with _ -> ()) o.option_class.class_hooks  


(* less: could also warn for config data without an option in the program *)
let really_load filename = 
  Logs.info (fun m -> m "loading options in %s" filename);
  let ic = open_in filename in
  let s = S.of_channel ic in
  try
    let stream = lexer s in
    let list_values = 
      try parse stream 
      with e -> 
        Printf.printf "At character pos %d (token %d)\n" 
           (S.count s) (S.count stream);
        raise e 
     in
    !options |> List.iter (fun o ->
        try
          o.option_value <- o.option_class.from_value
            (find_value o.option_name list_values);
          exec_chooks o;
          exec_hooks o;
        with Not_found -> () (* no error if option is not defined here *)
    )
  with e -> 
    Logs.err (fun m -> m "Error %s in %s\n" (Printexc.to_string e) filename)

      
let load () =
  try
    really_load !filename
  with _ ->
    Logs.info (fun m -> m  "No %s found\n" !filename)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let (!!) o = o.option_value
let (=:=) o v = 
  o.option_value <- v;
  exec_chooks o;
  exec_hooks o

      
let shortname o = String.concat ":" o.option_name
let get_type o = o.option_class
let get_help o = 
  let help = o.option_help in
  if help = "" then "No Help Available" else help

(*****************************************************************************)
(* Conversions (readable marshalling *)
(*****************************************************************************)

let value_to_string v =
  match v with Value s -> s | _ -> raise Not_found
let string_to_value s = 
  Value s
  
let value_to_int v =
  match v with Value s -> int_of_string s | _ -> raise Not_found  
let int_to_value i = 
  Value (string_of_int i)

(* The Pervasives version is too restrictive *)
let bool_of_string s = match String.lowercase_ascii s with
  | "true" -> true
  | "false" -> false
  | "yes" -> true
  | "no" -> false
  | "1" -> true
  | "0" -> false
  | _ -> invalid_arg "bool_of_string"

let value_to_bool v =
  match v with Value s -> bool_of_string s | _ -> raise Not_found
let bool_to_value i = 
  Value (string_of_bool i)

let value_to_float v =
  match v with Value s -> float_of_string s | _ -> raise Not_found
let float_to_value i = 
  Value (string_of_float i)

let value_to_string2 v =
  match v with List [Value s1; Value s2] -> s1,s2 | _ -> raise Not_found
let string2_to_value (s1,s2) = 
  SmallList [Value s1; Value s2]

let value_to_list v2c v =
  match v with List l -> List.map v2c l | _ -> raise Not_found
let list_to_value c2v l = List (
    List.fold_right (fun v list ->
        try (c2v v) :: list with _ -> list
    )  l [])
  
let smalllist_to_value c2v l = SmallList (
    List.fold_right (fun v list ->
        try (c2v v) :: list with _ -> list
    )  l [])
  
let value_to_path v =
  List.map Utils.string_to_filename
    (match v with Value s -> Utils.string_to_path s
    | List l -> List.map (fun v -> match v with
              Value s -> Utils.string_to_filename s | _ -> raise Not_found) l
    | _ -> raise Not_found)
let path_to_value list = 
  Value (Utils.path_to_string 
      (List.map Utils.filename_to_string list))

let tuple2_to_value (c1,c2) (a1,a2) =
  SmallList [to_value c1 a1; to_value c2 a2]
let value_to_tuple2 (c1,c2) v = 
  match v with
    List [v1;v2] -> (from_value c1 v1, from_value c2 v2)
  | _ -> raise Not_found

  
let tuple3_to_value (c1,c2,c3) (a1,a2,a3) =
  SmallList [to_value c1 a1; to_value c2 a2; to_value c3 a3]
let value_to_tuple3 (c1,c2,c3) v = 
  match v with
    List [v1;v2;v3] -> (from_value c1 v1, from_value c2 v2, from_value c3 v3)
  | _ -> raise Not_found

     
let value_to_filename v =  
  Utils.string_to_filename (match v with
      Value s -> s | _ -> raise Not_found)
let filename_to_value v = 
  Value (Utils.filename_to_string v)

let value_to_sum l v =
  match v with
    Value s -> List.assoc s l | _ -> raise Not_found
let sum_to_value l v = 
  Value (List.assq v l)

(*****************************************************************************)
(* Common option types *)
(*****************************************************************************)
  
let bool_option   = define_type "Bool" value_to_bool bool_to_value
let int_option    = define_type "Int" value_to_int int_to_value
let float_option  = define_type "Float" value_to_float float_to_value
let string_option = define_type "String" value_to_string string_to_value

let color_option  = define_type "Color" value_to_string string_to_value
let font_option   = define_type "Font" value_to_string string_to_value
let path_option   = define_type "Path" value_to_path path_to_value
let string2_option = define_type "String2" 
  value_to_string2 string2_to_value
let filename_option = define_type "Filename" 
  value_to_filename filename_to_value

 
let list_option cl = 
  define_type (cl.class_name ^ " List")
  (value_to_list cl.from_value) (list_to_value cl.to_value)

let smalllist_option cl = 
  define_type (cl.class_name ^ " List")
  (value_to_list cl.from_value) (smalllist_to_value cl.to_value)

let tuple2_option p = define_type "tuple2_option" 
     (value_to_tuple2 p) (tuple2_to_value p)

let tuple3_option p = define_type "tuple3_option" 
     (value_to_tuple3 p) (tuple3_to_value p)

let sum_option l = 
  let ll = List.map (fun (a1,a2) -> a2,a1) l in
  define_type "Sum" (value_to_sum l) (sum_to_value ll)
  
(*****************************************************************************)
(* Saving *)
(*****************************************************************************)

let exit_exn = Exit
let safe_string s =
  if s = "" then "\"\"" else
  try
    match s.[0] with
      'a' .. 'z' | 'A' .. 'Z' ->
        for i = 1 to String.length s - 1 do
          match s.[i] with
            'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> ()
          | _ -> raise exit_exn
        done; s 
    | '0' .. '9' ->
        for i = 1 to String.length s - 1 do
          match s.[i] with
            '0' .. '9' | '.' -> ()
          | _ -> raise exit_exn
        done; s 
    | _ -> raise exit_exn
  with _ ->
      Printf.sprintf "\"%s\"" (String.escaped s)

let with_help = ref false
      
let rec save_module indent oc list = 
  let subm = ref [] in
  List.iter (fun (name, help, value) ->
      match name with
        [] -> assert false
      | [ name ] -> 
          if !with_help && help <> "" then
            Printf.fprintf oc "(* %s *)\n" help;
          Printf.fprintf oc "%s %s = " indent (safe_string name);
          save_value indent oc value;
          Printf.fprintf oc "\n";
      | m :: tail ->
          let p = try List.assoc m !subm
            with _ -> 
                let p = ref [] in
                subm := (m, p) :: !subm;
                p in
          p := (tail, help, value) :: !p) list;
  List.iter (fun (m, p) ->
      Printf.fprintf oc "%s %s = {\n" indent (safe_string m);
      save_module (indent ^ "  ") oc !p;
      Printf.fprintf oc "%s}\n" indent
  ) !subm

and save_list indent oc list =
  match list with
    [] -> ()
  | [v] -> save_value indent oc v;
  | v :: tail ->
      save_value indent oc v;
      Printf.fprintf oc ", ";
      save_list indent oc tail

and save_list_nl indent oc list =
  match list with
    [] -> ()
  | [v] -> 
      Printf.fprintf oc "\n%s" indent;
      save_value indent oc v;
  | v :: tail ->
      Printf.fprintf oc "\n%s" indent;
      save_value indent oc v;
      Printf.fprintf oc ";";
      save_list_nl indent oc tail
      
and save_value indent oc v =
  match v with
    Value s -> 
      Printf.fprintf oc "%s" (safe_string s)
  | List l -> 
      Printf.fprintf oc "[";
      save_list_nl (indent ^ "  ") oc l;
      Printf.fprintf oc "]";
  | SmallList l -> 
      Printf.fprintf oc "(";
      save_list (indent ^ "  ") oc l;
      Printf.fprintf oc ")";
  | Module _ -> 
      Printf.fprintf oc "\"\""
    
let save () =
  let oc = open_out !filename in
  save_module "" oc (List.map (fun o -> 
        o.option_name, 
        o.option_help,
        try o.option_class.to_value o.option_value 
        with e -> 
            Printf.printf "Error while saving option \"%s\": %s" 
              (try List.hd o.option_name with _ -> "???") (Utils.printexn e);
            print_newline ();
            Value "") 
    (List.rev !options));
  close_out oc

let save_with_help () =
  with_help := true;
  (try save () with _ -> ());
  with_help := false

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)
  
let option_hook option f =
  option.option_hooks <- f :: option.option_hooks
  
let type_hook option_class f =
  option_class.class_hooks <- f :: option_class.class_hooks


let rec iter_order f list =
  match list with [] -> () | v :: tail -> f v; iter_order f tail
  
let help oc =
  List.iter (fun o ->
      Printf.fprintf oc"OPTION \"";
      (match o.option_name with
          [] -> Printf.fprintf oc "???"
        | [name] -> Printf.fprintf oc "%s" name
        | name :: _tail ->
            Printf.fprintf oc "%s" name;
            iter_order (fun name -> 
                Printf.fprintf oc ":%s" name
                ) o.option_name);
      Printf.fprintf oc "\" (TYPE \"%s\"): %s\n   CURRENT: \n" 
      o.option_class.class_name o.option_help;
      (try save_value "" oc (o.option_class.to_value o.option_value) 
        with _ -> ());
      Printf.fprintf oc "\n"
  ) !options;
  flush oc
