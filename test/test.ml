let () =
  let rec test_case connectnion =
    Result.bind
      (Nosock.Connection.recieve connectnion)
      (fun (f, connectnion) ->
      match f.opcode with
      | Nosock.Opcode.Close ->
        Result.ok ()
      | Nosock.Opcode.Pong -> test_case connectnion
      | Nosock.Opcode.Ping ->
        Nosock.Connection.send connectnion (Nosock.Frame.create Nosock.Opcode.Pong f.payload);
        test_case connectnion
      | _ ->
        Nosock.Connection.send connectnion f;
        test_case connectnion)
  in
  let check_case i =
    let connectnion = Nosock.Connection.create "127.0.0.1" 9001 in
    let connect =
      Nosock.Connection.connect
        connectnion
        (Printf.sprintf "/getCaseStatus?agent=nosock&case=%i" i)
    in
    let frame, connectnion = Nosock.Connection.recieve connectnion |> Result.get_ok in
    match frame.payload with
    | Nosock.Frame.Text response ->
      if String.equal response "{\"behavior\": \"FAILED\"}"
      then failwith (Printf.sprintf "bad case status %i" i)
      else print_endline response
    | Nosock.Frame.Bytes _ -> failwith "unexpected payload type"
  in
  let connectnion = Nosock.Connection.create "127.0.0.1" 9001 in
  List.iter
    (fun i ->
      let connect =
        Nosock.Connection.connect connectnion (Printf.sprintf "/runCase?case=%i&agent=nosock" i)
      in
      (match connect with
       | Result.Ok _ ->
        test_case connectnion |> ignore;
        Out_channel.close connectnion.out_channel;
       | Result.Error e ->
         (match e with
          | ParsingHeader {header_line = header_line} ->
            failwith (Printf.sprintf "Parsing header line \"%s\"" header_line)
          | SecKeyDoesntExists -> failwith "SecKey does not exists in headers"
          | InvalidSecKey -> failwith "Invalid sec-key"
          | HttpEndOfFile -> failwith "EOF"));
      check_case i)
    (List.init 301 (fun i -> i + 1));
    Nosock.Connection.connect connectnion "updateReports?agent=nosock" |> ignore
;;