(* 
module Mpost = struct

let rec no_extension f =
  try
    no_extension (Filename.chop_extension f)
  with Invalid_argument \"Filename.chop_extension\" -> f

let name =
  let name = ref (no_extension (Filename.basename Sys.argv.(0))) in
  for i = 1 to Array.length Sys.argv - 2 do
    if Sys.argv.(i) = \"-name\" then
      name := Sys.argv.(i+1)
  done;
  !name

let next_name =
  let cnt = ref 0 in
  fun () ->
    incr cnt;
    Printf.sprintf \"%s-melt-figure%d\" name !cnt

let mlpost_gen includegraphics ?(mode = mode) ?file f =
  let file = match file with
    | None -> next_name ()
    | Some file -> file
  in
  let ext = (*match mode with
    | `Pdf -> \".mps\"
    | `Ps ->*) \".pdf\" (*
    | `Cairo -> \".pdf\"
    | `Mps -> \".mps\"*)
  in
  let full_name = file ^ ext in
  Mlpost.Metapost.emit file f;
  includegraphics (Latex.text full_name)

let mlpost = mlpost_gen Latex.includegraphics

end 
open Mpost

*)


(* ****************************************************************** *)
(* ****************************************************************** *)
(* ****************************************************************** *)




(*

; subsection "Overview : the introduction of SH4 in the current architecture"
; 
"{let open Mlpost in
let open Tree in
let open Box in
let open Picture in
let open Num in
mlpost (
let state = Box.tex ~style:Circle ~stroke:(Some Color.black) in
let final = Box.box ~style:Circle in
let states = Box.vbox ~padding:(cm 0.8)
[Box.hbox ~padding:(cm 1.4)
  [state ~name:\"alpha\" \"aa\";
   state ~name:\"beta\" \"bb\" ];
   final ~name:\"gamma\" (state \"gg\") ] in
Box.draw states)
}"

*)












(* ****************************************************************** *)
(* ****************************************************************** *)
(* ****************************************************************** *)







module Box =
struct
  type 'a file = 
    | S_human (* source human *)
    | S_gen (* source generated *)
    | S_gen_field of 'a
    | Ast

  type id = string

  type direction = From | To

  module Tag =
  struct
    type tag = 
      | I of unit
      | Rec of tag
    type c = Compound of tag
    type b = Box of tag
  end

  type 'a node = 
    | Compound of Tag.c * Latex.t * 'a node list
    | Compound_ of Latex.t * 'a node list
    | Compound_other1 of Latex.t * 'a node list
    | Compound_other1_ of Tag.c * Latex.t * 'a node list
    | Compound_other2 of Latex.t * 'a node list
    | Compound_noname of Tag.c * 'a node list

    | Box of 'a file * Latex.t
    | Boxl of 'a file * Latex.t list
    | Box_ of Tag.b * 'a file * Latex.t
    | Boxl_ of Tag.b * 'a file * Latex.t list

    | Invisible_dot

  type 'a edge = 
    | Dir of 'a (* draw an edge (from, to) *)

    | Uni of 'a (* draw an unidirectional edge *)
    | Uni_round of 'a (* draw an unidirectional edge, starting and ending with a round shape *)
    | Uni_dotted of 'a (* draw an unidirectional edge, dotted style *)

  
  module Make (M : sig type t val f : Tag.tag -> t val destruct : t -> Tag.tag end) = 
  struct
    let pdf, txt, arm, sh, simgen, th, coq_simlight, c_simlight_arm, c_simlight, c_parser, c_compcert, c_etc, invisible, compcert_c_arm_man, semfun_arm_man, pretty_print = 
      let tag () = M.f (Tag.I ()) in
      tag (), tag (), tag (), tag (), tag (), tag (), tag (), tag (), tag (), tag (), tag (), tag (), tag (), tag (), tag (), tag ()

    let dec_inst x = M.f (Tag.Rec (M.destruct x))
  end

  module Tc = Make (struct open Tag type t = c let f x = Compound x let destruct (Compound x) = x end)
  module Tb = Make (struct open Tag type t = b let f x = Box x let destruct (Box x) = x end)

  type ('a, 'b) path = P of 'a list * 'b


  let node =
    [ Compound (Tc.pdf, "PDF", 
                [ Box_ (Tb.arm, S_human, "ARM manual")
                ; Box_ (Tb.sh, S_human, "SH manual") ])

    ; Compound (Tc.txt, "TXT extracted",
                [ Box_ (Tb.arm, S_gen, "ARM manual") 
                ; Box_ (Tb.sh, S_gen, "SH manual") ])

    ; Compound_noname (Tc.dec_inst Tc.arm, 
                       [ Box (Ast, "decoder") 
                       ; Box (S_gen, "instruction") ])

    ; Boxl_ (Tb.dec_inst Tb.sh, Ast, [ "instruction (contains the type Cparser)" ; "+" ; "decoder" ])

    ; Box_ (Tb.simgen, Ast, "pseudo-code generator")

    ; Compound_ ("Coq source", [ Boxl_ (Tb.th, S_human, [ "Theorem :" ; "deep equiv shallow ?" ])
                               ; Compound_ ("compiles with compcert", 
                                            [ Box (S_gen_field "ARM manual", "compcert c")
                                            ; Compound_other1 ("compcert source", [Boxl (S_human, [ "compcert c" ; "deep embedding" ])]) ])
                               ; Compound_other1_ (Tc.coq_simlight, "simulator", 
                                                   [ Boxl (S_gen, [ "semfun" ; "shallow embedding" ]) 
                                                   ; Box_ (Tb.arm, S_gen_field "ARM manual", "semfun")
                                                   ; Box_ (Tb.sh, S_gen_field "SH manual", "semfun") ]) ])

    ; Compound_other2 ("compcert AST", [ Boxl (Ast, [ "cparser" ; "x semantics" ])
                                       ; Boxl (Ast, [ "compcert c" ; "v semantics" ]) ])

    ; Compound_ ("c source", [ Compound_other1 ("simlight", [ Box_ (Tb.c_simlight_arm, S_gen, "ARM manual")]) ])

    ; Invisible_dot ]

  let edge = 
    let path_all_ml = 
      let rec aux x0 = function
        | x1 :: xs -> Dir (x0, x1) :: if xs = [] then [] else aux x1 xs
        | _ -> assert false in
      function x :: xs -> aux x xs | _ -> assert false in

    let path_unidir = 
      let rec aux x0 = function
        | [x1] -> [ Dir (x0, x1) ]
        | x1 :: xs -> Uni (x0, x1) :: aux x1 xs in
      function x :: xs -> aux x xs | _ -> assert false in

    let pnil x = P ([], x) in

    let before_simgen_path arm = [ P ([Tc.pdf], arm) ; P ([Tc.txt], arm) ; pnil (Tb.dec_inst arm) ; pnil Tb.simgen ] in

    let single = List.map pnil in

    BatList.flatten 
      [ path_all_ml (before_simgen_path Tb.arm)
      ; path_all_ml (before_simgen_path Tb.sh)

      ; List.map (fun x -> Dir (pnil Tb.simgen, x)) [ P ([Tc.coq_simlight], Tb.arm) ; P ([Tc.coq_simlight], Tb.sh) ; pnil Tb.c_simlight_arm ] (* (simgen) -> bleu *)

      ; path_all_ml (single [ Tb.c_simlight ; Tb.c_parser ; Tb.c_compcert ; Tb.c_etc ]) (*  *)

      ; path_unidir (single [ Tb.c_simlight_arm ; Tb.c_parser ; Tb.c_compcert ; Tb.invisible ; Tb.compcert_c_arm_man ]) (* marron *)

      ; List.map (fun x -> Uni_round (pnil Tb.th, pnil x)) [ Tb.semfun_arm_man ; Tb.compcert_c_arm_man ]

      ; [ Uni_dotted (pnil Tb.invisible, pnil Tb.pretty_print) ] ]
end

