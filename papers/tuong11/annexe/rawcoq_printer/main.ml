open V
open Datatypes
open BinPos
open Ascii
open Printf

module String =
struct
  include String
  let make n c =  
    let rec aux acc n = if n = 0 then acc else aux (sprintf "%s%c" acc c) (pred n) in
    aux "" n
end

let to_char (Ascii (b0, b1, b2, b3, b4, b5, b6, b7)) =
  let f i b = 
    let rec aux acc n = if n = 0 then acc else aux (acc * 2) (pred n) in aux 1 i * (if b = Coq_true then 1 else 0) in
  char_of_int (f 0 b0 + f 1 b1 + f 2 b2 + f 3 b3 + f 4 b4 + f 5 b5 + f 6 b6 + f 7 b7)

let to_string = 
  let rec aux = function
    | String.EmptyString -> ""
    | String.String (c, s) -> String.make 1 (to_char c) ^ aux s in
  aux

let of_l = 
  let rec aux = function
    | [] -> Coq_nil
    | a :: l -> Coq_cons (a, aux l) in
  aux

let map f =
  let rec aux = function [] -> [] | x :: xs -> f x :: aux xs in
  aux

let _ = 
  Printf.printf "Check %s.\n%!" (to_string (C._ast { prog_funct = of_l (map 
                                                                   (fun a -> Coq_pair (Coq_xH, a))
                                                                   [Tvoid ; Tfloat F32])
                                            ; prog_main = Coq_xH }))
