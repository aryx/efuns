(***********************************************************************)
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

type reader = {
  fd : Unix.file_descr;
  thread : Thread.t;
  (* old: we were using Thread.exit() and Thread.kill() before but
   * they got deprecated with OCaml 5 and OCaml 4.14 so we need
   * to switch to different thread killing strategy hence this
   * bool
   * LATER: use Atomic.t bool at some point 
   *)
  stop : bool ref;
}

let readers : reader list ref = ref []

(* protected-by: mu_actions *)    
let actions : int ref = ref 0
let mu_actions : Mutex.t = Mutex.create ()
let cond_actions : Condition.t = Condition.create ()

let add_reader fd f = 
  let stop = ref false in
  let thread =
    Thread.create 
      (fun () ->
        Logs.debug (fun m -> m "add_reader started thread %d" 
              (Thread.id (Thread.self ())));
        while not !stop do
          Mutex.lock mu_actions;
          incr actions;
          Condition.signal cond_actions;
          Mutex.unlock mu_actions;
          let _ = Unix.select [fd] [] [] (-0.1) in
          try
            f ()
          with 
          | Thread.Exit as exn ->
              let e = Exception.catch exn in
              Logs.debug (fun m -> m "add_reader exiting (thread %d)"
                  (Thread.id (Thread.self ())));
              Exception.reraise e
          | e -> 
            Logs.err (fun m -> m "add_reader: exn %s (thread %d)"
                  (Common.exn_to_s e) (Thread.id (Thread.self ())))
        done) ()
  in
  readers := { fd ; thread; stop } :: !readers

let remove_reader fd = 
  Logs.debug (fun m -> m "remove_reader (thread %d)" (Thread.id (Thread.self())));
  Mutex.lock mu_actions;
  let me = ref false in
  let rec iter lst res to_stop =
    match lst with
    | [] -> res, to_stop
    | r :: tail ->
        if r.fd = fd then 
          if r.thread == Thread.self () then (
              me := true;
              iter tail res to_stop
           ) else
              iter tail res (r :: to_stop)
          else
            iter tail ( r :: res ) to_stop
  in
  let (res, to_stop) = iter !readers [] [] in
  readers := res;
  to_stop |> List.iter (fun r ->
    (* old: Thread.kill r.th
     * but Thread.kill was deprecated in OCaml 4.14 and OCaml 5 and
     * was apparently not reliable anyway before so thx to ChatGPT
     * I switched to the stop boolean approach.
     *)
     r.stop := true     
  );
  Mutex.unlock mu_actions;
  if !me then 
    (* old: Thread.exit (), but deprecated in OCaml 5 and incorrect in OCaml 4 => 
     * raise Thread.Exit instead
     *)
    raise Thread.Exit


(* old: not used anymore inside efuns

let iterator lst_it =
  Mutex.lock mu_actions;
  while !actions = !lst_it do
    Condition.wait cond_actions mu_actions;
  done;
  lst_it := !actions;
  Mutex.unlock mu_actions


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

let poll () = false

external ioctl: Unix.file_descr -> int -> string -> 'a -> unit = "ml2c_ioctl"

*)
