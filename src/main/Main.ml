(*s: Main.ml *)
(*s: toplevel [[Main]] call [[main()]] *)
let _ =
  (*old: UCommon.main_boilerplate (fun () -> *)
  Cap.main (fun (caps : Cap.all_caps) ->
    (* let r = Gc.get () in r.Gc.verbose <- true; Gc.set r; *)
    let argv = CapSys.argv caps in
    Exit.exit caps
      (Exit.catch (fun () ->
         CLI.main caps argv))
  )
(*e: toplevel [[Main]] call [[main()]] *)
(*e: Main.ml *)
