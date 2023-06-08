open Error

let decode_utf_8 (b : bytes) =
  let buff = Buffer.create 0 in
  let rec decode_recode pos =
    if pos = Bytes.length b
    then Result.ok @@ Buffer.contents buff
    else (
      let decode = Bytes.get_utf_8_uchar b pos in
      if Uchar.utf_decode_is_valid decode |> Bool.not
      then Result.error DecodeText
      else (
        let char = Uchar.utf_decode_uchar decode in
        Buffer.add_utf_8_uchar buff char;
        decode_recode (pos + Uchar.utf_decode_length decode)))
  in
  decode_recode 0
;;

type t = {
  host : string;
  port : int;
  in_channel : In_channel.t;
  out_channel : Out_channel.t;
  buffer_tail : bytes;
  unfinished_data_frame : (Opcode.t * bytes list) option;
}

let rec range (n : int) : int Seq.t =
  let rec aux l () = if Int.equal l 0 then Seq.Nil else Seq.Cons (l, aux (l - 1)) in
  range 10
;;

(*

let create_test host port : t =
  let in_file_descr, out_file_descr = Unix.pipe () in
  let in_channel = Unix.in_channel_of_descr in_file_descr in
  let out_channel = Unix.out_channel_of_descr out_file_descr in
  { host; port; in_channel; out_channel }
;;
let create_ssl_connection host port : t =
  let inet_addr = make_addr host port in
  let socket = Ssl.open_connection Ssl.SSLv23 inet_addr in
  let file_descr = Ssl.file_descr_of_socket socket in
  let out_channel = Unix.out_channel_of_descr file_descr in
  let in_channel = Unix.in_channel_of_descr file_descr in
  { host; port; in_channel; out_channel }
;;
*)

let make_addr host port =
  let sockaddr =
    Unix.getaddrinfo
      host
      (string_of_int port)
      [
        Unix.AI_FAMILY Unix.PF_INET; Unix.AI_SOCKTYPE Unix.SOCK_STREAM; Unix.AI_PROTOCOL 6;
      ]
    |> fun a -> (List.nth a 0).ai_addr
  in
  match sockaddr with
  | ADDR_UNIX _ -> assert false
  | a -> a
;;

let create host port : t =
  let inet_addr = make_addr host port in
  let in_channel, out_channel = Unix.open_connection inet_addr in
  {
    host;
    port;
    in_channel;
    out_channel;
    buffer_tail = Bytes.empty;
    unfinished_data_frame = None;
  }
;;

let rec recv connection (required_len : int) (buffer : bytes)
  : (bytes * bytes, protocol_error) result
  =
  if Bytes.length buffer >= required_len
  then
    Result.ok
      ( Bytes.sub buffer 0 required_len,
        Bytes.sub buffer required_len (Bytes.length buffer - required_len) )
  else (
    let b = Bytes.create 1024 in
    let readed_length = In_channel.input connection.in_channel b 0 1024 in
    if readed_length = 0
    then Result.error ProtocolEndOfFile
    else (
      let data = Bytes.cat buffer (Bytes.sub b 0 readed_length) in
      if Bytes.length data >= required_len
      then
        Result.ok
          ( Bytes.sub data 0 required_len,
            Bytes.sub data required_len (Bytes.length data - required_len) )
      else recv connection required_len data))
;;

let get_frame (connection : t) (tail : bytes) =
  (* Read requiered length of data from buffer if it contains anough bytes. Otherwise it will be readed from connection*)
  let get_header tail =
    Result.bind (recv connection 2 tail) (fun (data, buffer_tail) ->
      let first, second = Bytes.get data 0 |> Char.code, Bytes.get data 1 |> Char.code in
      let fin = (first lsr 7) land 1 = 1 in
      let rsv1 = (first lsr 6) land 1 in
      let rsv2 = (first lsr 5) land 1 in
      let rsv3 = (first lsr 4) land 1 in
      if rsv1 > 0 || rsv2 > 0 || rsv3 > 0
      then Result.error @@ NonZeroReservedBit
      else (
        let opcode =
          Opcode.of_int (first land 0xF) |> Option.to_result ~none:WrongOpcode
        in
        Result.bind opcode (fun valid_opcode ->
          let opcode_int = Opcode.to_int valid_opcode in
          if opcode_int > 0x7 && Bool.not fin
          then Result.error FragmentedControlFrame
          else begin
            let has_mask = (second lsr 7) land 1 > 0 in
            let frame_data_length = second land 0x7F in
            Result.ok (fin, valid_opcode, has_mask, frame_data_length, buffer_tail)
          end)))
  in
  let calculate_length (length : int) (buffer : bytes) =
    match length with
    | i when i < 126 -> Result.ok (length, buffer)
    | 126 ->
      Result.bind (recv connection 2 buffer) (fun (data, buffer_tail) ->
        Result.ok (EndianBytes.BigEndian.get_uint16 data 0, buffer_tail))
    | _ ->
      Result.bind (recv connection 8 buffer) (fun (data, buffer_tail) ->
        Result.ok @@ (EndianBytes.BigEndian.get_int64 data 0 |> Int64.to_int, buffer_tail))
  in
  let unmask (data : bytes) (mask : bytes) =
    Bytes.mapi
      (fun i chr ->
        let rem = Float.rem (Float.of_int i) 4.0 |> Float.to_int in
        let r = Char.code chr lxor (Bytes.get mask rem |> Char.code) in
        Char.chr r)
      data
  in
  Result.bind (get_header tail) (fun (fin, opcode, has_mask, length, buffer) ->
    Result.bind (calculate_length length buffer) (fun (final_length, buffer) ->
      if final_length = 0
      then Result.ok ((fin, opcode, Bytes.empty), buffer)
      else if has_mask
      then
        Result.bind
          (recv connection (final_length + 4) buffer)
          (fun (data, buffer_tail) ->
            let mask = Bytes.sub data 0 4 in
            let data = Bytes.sub data 4 final_length in
            Result.ok ((fin, opcode, unmask data mask), buffer_tail))
      else
        Result.bind (recv connection final_length buffer) (fun (data, buffer_tail) ->
          Result.ok ((fin, opcode, data), buffer_tail))))
;;

let handle_close_message ((fin, opcode, payload) : bool * Opcode.t * bytes) =
  let close_codes =
    [ 1000; 1001; 1002; 1003; 1006; 1007; 1008; 1009; 1010; 1011; 1012; 1013; 1014 ]
  in
  if Bytes.length payload = 1
  then Result.error @@ WrongCloseCode
  else if Bytes.length payload >= 2
  then (
    let close_code = EndianBytes.BigEndian.get_uint16 (Bytes.sub payload 0 2) 0 in
    if close_code < 3000 && (Bool.not @@ List.exists (fun v -> v = close_code) close_codes)
    then Result.error @@ WrongCloseCode
    else if close_code = 1006
    then Result.error @@ WrongCloseCode
    else (
      let data = Bytes.sub payload 2 (Bytes.length payload - 2) in
      if Bytes.length data > 125
      then Result.error WrongCloseCode
      else if Bytes.length data = 0
      then Result.ok (fin, opcode, Bytes.empty)
      else (
        match decode_utf_8 data with
        | Result.Error p -> Result.error p
        | Result.Ok p -> Result.ok (fin, opcode, payload))))
  else Result.ok (fin, opcode, Bytes.empty)
;;

let connect (connection : t) url =
  Random.init (Unix.time () |> Float.to_int);
  let key = Http.generate_sec_key () in
  let req_handshake =
    Handshake.get_handshake_request connection.host connection.port url key
  in
  Out_channel.output_string connection.out_channel req_handshake;
  Out_channel.flush connection.out_channel;
  Result.bind (Http.read_response connection.in_channel) (fun response ->
    Handshake.validate response key)
;;

let print_bytes b =
  Bytes.iter (fun c -> print_string (Format.sprintf "%i " (Char.code c))) b
;;

let range s =
  let rec aux l () = if l = 0 then Seq.Nil else Seq.Cons (l, aux (l - 1)) in
  aux s
;;

let to_seq s =
  let rec aux i () = if i = 0 then Seq.Nil else Seq.Cons (i, aux (i - 1)) in
  aux s
;;

let send (connectnion : t) frame =
  Out_channel.output_bytes connectnion.out_channel (Frame.to_send frame true);
  Out_channel.flush connectnion.out_channel
;;

type unfinished_frame = Opcode.t * bytes list

let rec _recieve
  (connection : t)
  (tail : bytes)
  (unfinished_frame : unfinished_frame option)
  : (Frame.t * unfinished_frame option * bytes, protocol_error) result
  =
  let frame = get_frame connection tail in
  Result.bind frame (fun ((fin, opcode, payload), frame_tail) ->
    match opcode with
    | Opcode.Close ->
      (match handle_close_message (fin, opcode, payload) with
       | Result.Error e ->
         Out_channel.close connection.out_channel;
         Result.error e
       | Result.Ok (fin, opcode, payload) ->
        let frame = Frame.create opcode (Frame.Bytes payload) in
        send connection frame;
        Out_channel.close connection.out_channel;
         Result.ok (frame, unfinished_frame, frame_tail))
    | Opcode.Pong ->
      Result.ok (Frame.create opcode (Frame.Bytes payload), unfinished_frame, frame_tail)
    | Opcode.Ping ->
      if Bytes.length payload > 125
      then (
        Out_channel.close connection.out_channel;
        Result.error @@ PingPayloadToLarge)
      else
        Result.ok (Frame.create opcode (Frame.Bytes payload), unfinished_frame, frame_tail)
    | Opcode.Continuation ->
      (match unfinished_frame with
       | None ->
         Out_channel.close connection.out_channel;
         Result.error @@ FirstFrameIsContinuation
       | Some (unfin_opcode, unfin_chunks) ->
         (* unfinished_frame.payload <- (Bytes.cat unfinished_frame.payload f.payload); *)
         if fin
         then (
           let concated_data =
             Bytes.concat Bytes.empty (List.rev (payload :: unfin_chunks))
           in
           if unfin_opcode = Opcode.Text
           then (
             match decode_utf_8 concated_data with
             | Result.Ok p ->
               Result.ok
                 (Frame.create unfin_opcode (Frame.Text p), unfinished_frame, frame_tail)
             | Result.Error e ->
               Out_channel.close connection.out_channel;
               Result.error @@ e)
           else
             Result.ok
               ( Frame.create unfin_opcode (Frame.Bytes concated_data),
                 unfinished_frame,
                 frame_tail ))
         else
           _recieve connection frame_tail (Some (unfin_opcode, payload :: unfin_chunks)))
    | Opcode.Text ->
      if Option.is_some unfinished_frame
      then (
        Out_channel.close connection.out_channel;
        Result.error @@ FrameIsNotContinuation)
      else if fin
      then (
        match decode_utf_8 payload with
        | Result.Ok p ->
          Result.ok (Frame.create opcode (Frame.Text p), unfinished_frame, frame_tail)
        | Result.Error e -> Result.error e)
      else _recieve connection frame_tail (Some (Opcode.Text, [ payload ]))
    | Opcode.Binary ->
      if Option.is_some unfinished_frame
      then Result.error @@ FrameIsNotContinuation
      else if fin
      then
        Result.ok (Frame.create opcode (Frame.Bytes payload), unfinished_frame, frame_tail)
      else _recieve connection frame_tail (Some (Opcode.Binary, [ payload ])))
;;

let recieve (connection : t) : (Frame.t * t, protocol_error) result =
  Result.bind
    (_recieve connection connection.buffer_tail connection.unfinished_data_frame)
    (fun (frame, unfinished_frame, tail) ->
    Result.ok
      ( frame,
        {
          host = connection.host;
          port = connection.port;
          in_channel = connection.in_channel;
          out_channel = connection.out_channel;
          buffer_tail = tail;
          unfinished_data_frame = unfinished_frame;
        } ))
;;
(*
let ffffffffff () =
  let connectnion = Connection.create "127.0.0.1" 8765 in
  let connect = Connection.connect connectnion "" in
  match connect with
  | Result.Ok _ -> recieve_loop connectnion Bytes.empty None
  | Result.Error e ->
    (match e with
     | `ConnecationClose -> print_endline "ConnecationClose"
     | `InvalidSecKeyError s -> print_endline s
     | ` s -> print_endline s
     | `SecKeyDoesntExistsError -> print_endline "SecKeyDoesntExistsError")
;;


(List.init 27 (fun i -> i + 1))
(Connection.recv connectnion 65535 Bytes.empty) |> Result.get_ok |> print_bytes;;
(Connection.recv connectnion 7240 Bytes.empty) |> Result.get_ok  |> Bytes.iter (fun c -> print_string (Format.sprintf "%i " (Char.code c)));  *)
