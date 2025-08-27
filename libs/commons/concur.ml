(***********************************************************************)
(*                                                                     *)
(*                    ______________                                   *)
(*                                                                     *)
(*      Fabrice Le Fessant, projet SOR/PARA INRIA Rocquencourt         *)
(*                                                                     *)
(*                 mail: fabrice.le_fessant@inria.fr                   *)
(*                 web:  http://www-sor.inria.fr/~lefessan             *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*                                                                     *)
(***********************************************************************)

(*         File written in the Objective-CAML language                 *)

(* external ioctl: Unix.file_descr -> int -> string -> 'a -> unit = "ml2c_ioctl" *)

let readers = ref []
    
    let actions = ref 0
    let mu_actions = Mutex.create ()
    let cond_actions = Condition.create ()
    let add_reader fd f = readers := 
       (fd,
        Thread.create 
          (fun () ->
            while true do
              Mutex.lock mu_actions;
              incr actions;
              Condition.signal cond_actions;
              Mutex.unlock mu_actions;
              let _ = Unix.select [fd] [] [] (-0.1) in
              try
                f ()
              with e -> 
                print_string (Printexc.to_string e);
                print_newline ()
            done) ()) :: !readers
    let remove_reader fd = 
      Mutex.lock mu_actions;
      let me = ref false in
      let rec iter list res to_kill =
        match list with
          [] -> res, to_kill
        | ((fd',th) as ele):: tail ->
            if fd = fd' then 
              iter tail res 
                (if th == Thread.self () then
                  (me := true; to_kill)
                else th :: to_kill)
            else 
              iter tail (ele :: res) to_kill
      in
      let (res, to_kill) = iter !readers [] [] in
      readers := res;
      (* List.iter Thread.kill to_kill; *)
      UCommon.pr2 "TODO: Thread.kill not available anymore";
      ignore to_kill;
      Mutex.unlock mu_actions;
      if !me then 
        (* old: Thread.exit (), but deprecated in OCaml 5 => raise instead *)
        raise Thread.Exit

    let add_timer time f =
      let _ = Thread.create (fun _ -> Thread.delay time; f ()) () in ()

    let fork () =
      let pid = Unix.fork () in
      if pid > 0 then
        ignore (Thread.create (fun _ -> 
                    (* old: let _ = Thread.wait_pid pid in, but deprecated Ocaml5 *)
                    let _ = Unix.waitpid [] pid in
                    ()) ());
      pid

  
  

let iterator lst_it =
  Mutex.lock mu_actions;
  while !actions = !lst_it do
    Condition.wait cond_actions mu_actions;
  done;
  lst_it := !actions;
  Mutex.unlock mu_actions

let poll () = false

