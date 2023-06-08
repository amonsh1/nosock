let generate_sec_key () =
  String.init 16 (fun _ -> Char.chr (Random.int 256)) |> Base64.encode |> Result.get_ok
;;

let read_response (i_channel : In_channel.t) =
  let rec _read_headers (response : (string * string) list) =
    match In_channel.input_line i_channel with
    | Some r when String.equal (String.trim r) "" ->
      Result.ok @@ Hashtbl.of_seq @@ List.to_seq response
    | Some r ->
      (match r |> String.trim |> Str.split (Str.regexp ":") with
       | key :: value ->
         _read_headers ((key, String.trim (String.concat "" value)) :: response)
       | _ -> Result.error (Error.ParsingHeader {header_line = r}))
    | None -> Result.error @@ Error.HttpEndOfFile
  in
  let rec _read_start_line () =
    match In_channel.input_line i_channel with
    | Some r when String.equal (String.trim r) "" -> _read_start_line ()
    | Some r -> Result.ok (String.trim r)
    | None -> Result.error Error.HttpEndOfFile
  in
  match _read_start_line () with
  | Ok line ->
    (match _read_headers [] with
     | Ok headers -> Result.ok (line, headers)
     | Error e -> Error e)
  | Error e -> Error e
;;
