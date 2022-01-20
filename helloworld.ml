let ( $ ) f y = f y
let alphabet : char array =  
  Array.of_seq $ String.to_seq "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

let find_index a _as = 
  let rec go n b bs = 
    if bs.(n) = b
      then n 
      else go (n + 1) b bs
    in go 0 a _as

let (%) (x : int) (y : int) : int =
  let result = x mod y 
  in if result >= 0 
      then result
      else result + y

let encode_char (n : int) (a : char) : char = 
  match a with
    ' ' -> ' '
  | _ -> 
    let m = (n + find_index a alphabet) % 26
    in Array.get alphabet m

let decode_char (n : int) (a : char) : char = 
  match a with
    ' ' -> ' '
  | _ -> 
    let m = (find_index a alphabet - n) % 26
    in Array.get alphabet m

let encode (n : int) (s : string) : string = 
  String.of_seq (Seq.map (encode_char n) $ String.to_seq s)

let decode (n : int) (s : string) : string = 
  String.of_seq (Seq.map (decode_char n) $ String.to_seq s)

let rec range (a : int) (b : int) : int list = 
  if a = b 
    then [] 
    else List.cons a (range (a + 1) b) 

let decode_all (s : string) : unit = 
  let nums = range 0 26
  in List.iter (fun n -> print_endline $ decode n s) nums 

let () = decode_all "DWZH AZTYE LNNPDD MPYNS"