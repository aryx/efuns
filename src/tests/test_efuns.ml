let () =
  Testo.interpret_argv ~project_name:"efuns" (fun _env ->
    List.flatten [
      Unit_text.tests ();
      Unit_frame.tests ();
    ]
  )
