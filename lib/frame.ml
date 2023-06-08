type data_type =
  | Bytes of bytes
  | Text of string

type t = {
  opcode : Opcode.t;
  payload : data_type;
}

let create opcode payload : t = { opcode; payload }

let to_string { opcode; payload } =
  let payload =
    match payload with
    | Bytes b -> b
    | Text t -> Bytes.of_string t
  in
  Printf.sprintf
    "opcode = %i\npayloads = %s\n"
    (Opcode.to_int opcode)
    (payload
    |> Bytes.to_seq
    |> Seq.map (fun g -> Char.code g |> Int.to_string)
    |> List.of_seq
    |> String.concat "")
;;

let mask_payload (payload : bytes) : bytes * bytes =
  let r = Random.full_int 0xFFFFFFFF in
  let mask = Bytes.create 4 in
  Bytes.set_int32_be mask 0 @@ Int32.of_int r;
  let mask =
    Bytes.of_seq (List.to_seq [ Char.chr 176; Char.chr 240; Char.chr 203; Char.chr 66 ])
  in
  let masked_data =
    Bytes.mapi
      (fun i ch -> Char.code ch lxor Char.code (Bytes.get mask (i mod 4)) |> Char.chr)
      payload
  in
  mask, masked_data
;;

let to_send ({ opcode; payload } : t) (use_mask : bool) : bytes =
  let payload =
    match payload with
    | Bytes b -> b
    | Text t -> Bytes.of_string t
  in
  let mask_bit = if use_mask then 0x80 else 0 in
  let header =
    match Bytes.length payload with
    | l when l < 126 ->
      let length = Bytes.create 1 in
      Bytes.set_int8 length 0 (l lor mask_bit);
      Bytes.concat
        Bytes.empty
        [ Bytes.make 1 (Char.chr (0x80 lor 0 lor Opcode.to_int opcode)); length ]
    | l when l < 65536 ->
      let length = Bytes.create 2 in
      Bytes.set_int16_be length 0 l;
      Bytes.concat
        Bytes.empty
        [
          Bytes.make 1 (Char.chr (0x80 lor 0 lor Opcode.to_int opcode));
          Bytes.make 1 (Char.chr (0x7e lor mask_bit));
          length;
        ]
    | l ->
      let length = Bytes.create 8 in
      Bytes.set_int64_be length 0 @@ Int64.of_int l;
      Bytes.concat
        Bytes.empty
        [
          Bytes.make 1 (Char.chr (0x80 lor 0 lor Opcode.to_int opcode));
          Bytes.make 1 (Char.chr (0x7f lor mask_bit));
          length;
        ]
  in
  if use_mask
  then (
    let mask, payload = mask_payload payload in
    Bytes.cat header (Bytes.cat mask payload))
  else Bytes.cat header payload
;;
