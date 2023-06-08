type t = {
  first_line : string;
  headers : (string, string) Hashtbl.t;
}

let to_string ({ first_line; headers } : t) =
  Hashtbl.to_seq headers
  |> Seq.map (fun (key, value) -> Printf.sprintf "%s:%s" key value)
  |> List.of_seq
  |> String.concat "\r\n"
  |> Printf.sprintf "%s\r\n%s" first_line
;;

(** [get_handshake_request host port url sec_key] *)
let get_handshake_request host port url sec_key : string =
  let headers : (string * string) list =
    [
      "Host", Printf.sprintf "%s:%i" host port;
      "Connection", "Upgrade";
      "Pragma", "no-cache";
      "Cache-Control", "no-cache";
      "Upgrade", "websocket";
      "Sec-WebSocket-Version", "13";
      "Sec-WebSocket-Key", sec_key;
    ]
  in
  List.map (fun (k, v) -> Format.sprintf "%s:%s" k v) headers
  |> String.concat "\r\n"
  |> Printf.sprintf "GET %s HTTP/1.1\r\n%s\r\n\r\n" url
;;

let validate ((_, headers) : string * (string, string) Hashtbl.t) (key : string) =
  let request_key =
    Sha1.to_bin (Sha1.string (key ^ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"))
    |> Base64.encode
    |> Result.get_ok
  in
  let response_key = Hashtbl.find_opt headers "Sec-WebSocket-Accept" in
  match response_key with
  | None -> Result.error Error.SecKeyDoesntExists
  | Some k ->
    (match String.equal request_key k with
     | true -> Result.ok ()
     | false -> Result.error Error.InvalidSecKey)
;;
