type t =
  | Ping
  | Pong
  | Continuation
  | Text
  | Close
  | Binary

let to_string = function
  | Ping -> "ping"
  | Pong -> "pong"
  | Continuation -> "continuation"
  | Text -> "text"
  | Close -> "close"
  | Binary -> "binary"
;;

let of_int = function
  | 0 -> Some Continuation
  | 1 -> Some Text
  | 2 -> Some Binary
  | 8 -> Some Close
  | 9 -> Some Ping
  | 10 -> Some Pong
  | _ -> None
;;

let to_int = function
  | Continuation -> 0
  | Text -> 1
  | Binary -> 2
  | Close -> 8
  | Ping -> 9
  | Pong -> 10
;;
