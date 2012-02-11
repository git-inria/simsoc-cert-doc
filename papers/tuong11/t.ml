module BatList =
struct
  include BatList

  let nsplit f = 
    let rec aux acc (f1, f2) = function
      | [] -> List.rev acc
      | l -> aux (take_while f1 l :: acc) (f2, f1) (drop_while f1 l) in
    aux [] (f, (fun x -> not (f x)))
end

module L = (* latex missing functions *)
struct
  let (@) ?packages name ?opt (x, mode) = command ?packages name ?opt (BatList.map (fun x -> mode, x) x) mode

  module type COLOR =
  sig
    type color
    type 'a tup3

    val of_float_1 : float tup3 -> color
    val of_int_255 : int tup3 -> color

    val definecolor : color -> Latex.t
    val definecolor_used : (color -> Latex.t -> Latex.t) -> Latex.t -> Latex.t (* fold colors stored by [textcolor_] *)
    val definecolor_reset : unit -> unit (* empties the cache, behaves as if [textcolor_] has never been called *)
    val textcolor_ : color -> Latex.t -> Latex.t (* store color used, [definecolor_used] will fold it *)
(*    val textcolor : color -> Latex.t -> Latex.t (* nothing is stored *)*)
    val color_ : color -> Latex.t (* store color used, [definecolor_used] will fold it *)
    val color_name_ : color -> Latex.t (* store color used, [definecolor_used] will fold it *)
(*    val color_compare : color -> color -> int*)

    val comment : color
    val alpha_keyword : color
    val nonalpha_keyword : color
    val string : color
    val construct : color
    val black : color
    val green : color
    val grey : color
    val blue : color
    val yellow : color
    val violet : color
    val red : color
  end

  module Color : COLOR with type 'a tup3 = 'a * 'a * 'a =
  struct
    type 'a tup3 = 'a * 'a * 'a
    type raw_color = 
      | N_255 of int tup3
      | N_1 of float tup3

    module Float3Map = BatMap.Make (struct type t = float tup3  let compare = compare end)

    let n1_of_n255 (r, g, b) = 
      let f x = float x /. 255. in
      f r, f g, f b

    let raw_color = function
      | N_1 (r, g, b) -> r, g, b
      | N_255 (r, g, b) -> n1_of_n255 (r, g, b)

    let raw_color_compare a b = compare (raw_color a) (raw_color b)

    let get_color_id, fold = 
      let refe = ref (Float3Map.empty, 0) in
      (fun col -> 
        let map, dim_map = !refe in
        let o, i = 
          let col = raw_color col in
          match Float3Map.Exceptionless.find col map with
            | None -> 
              Some (Float3Map.add col dim_map map, Pervasives.succ dim_map), dim_map
            | Some i -> None, i in
        let _ = match o with None -> () | Some r -> refe := r in
        i, col), 
      fun f -> let map, _ = !refe in Float3Map.fold f map

    type color = int (* id *) * raw_color
    let color_compare (a, _) (b, _) = compare a b
    module ColorMap = BatMap.Make (struct type t = color let compare = color_compare end)

    let of_float_1 (r, g, b) = get_color_id (N_1 (r, g, b))
    let of_int_255 (r, g, b) = get_color_id (N_255 (r, g, b))

    module L =
    struct
      let definecolor name rgb x = \"definecolor\" @ ([ name ; rgb ; x ], A)
      let textcolor n x = \"textcolor\" @ ([ n ; x ], A)
      let color x =  \"color\" @ ([x], A)
    end
    let color_of_name x = text (Printf.sprintf \"color%d\" x)

    let definecolor (id, col) = 
      let msg, colo = 
        match col with
          | N_1 (r, g, b) -> "rgb", text (Printf.sprintf \"%f, %f, %f\" r g b)
          | N_255 (r, g, b) -> "RGB", text (Printf.sprintf \"%d, %d, %d\" r g b) in
      L.definecolor (color_of_name id) msg colo

    let textcolor (id, _) = L.textcolor (color_of_name id)
    let color (id, _) = L.color (color_of_name id)
    let color_name (id, _) = color_of_name id

    let definecolor_used, definecolor_reset, textcolor_, color_, color_name_ =
      let col_map = ref ColorMap.empty in
      (fun f -> ColorMap.fold (fun k _ -> f k) !col_map), 
      (fun _ -> col_map := ColorMap.empty),
      (fun c -> 
        let _ = col_map := ColorMap.add c () !col_map in
        textcolor c),
      (fun c -> 
        let _ = col_map := ColorMap.add c () !col_map in
        color c),
      (fun c -> 
        let _ = col_map := ColorMap.add c () !col_map in
        color_name c)

    let black = of_int_255 (let i = 0 in i, i, i)
    let green = of_int_255 (60, 179, 113)
    let grey = of_int_255 (*(0x24, 0xD3, 0xD5)*) (*(0, 139, 141)*) (*(0, 118, 39)*) (let i = 156 in i, i, i)
    let blue = of_int_255 (0, 5, 105)
    let yellow = of_int_255 (178, 121, 53)
    let violet = of_int_255 (206, 100, 255)
    let red = of_int_255 (200, 72, 0)
    module C = 
    struct
      open Caml2html.Output_latex

      let f = function
        | None, _ -> black
        | Some c, _ -> of_int_255 (let [ i1 ; i2 ; i3 ] = BatList.map int_of_string (BatString.nsplit c \",\") in i1, i2, i3)

      let comment = f comment_color
      let alpha_keyword = f alpha_keyword_color
      let nonalpha_keyword = f nonalpha_keyword_color
      let string = f string_color
      let construct = f construct_color
    end
    include C
  end

  module type TH =
  sig
    type label
    val newtheorem : ?opt:t -> t -> label * t
    val newtheorem' : ?opt:t -> t -> label * t
    val environment : ?packages:(string * string) list -> label -> ?opt:mode * t -> ?args:(mode * t) list -> mode * t -> mode -> t
    val env : label -> t -> t (* shortcut *)
  end

  module Th : TH =
  struct
    open Printf

    type label = L of int

    let mk_new = 
      let r = ref 0 in
      fun _ -> let _ = incr r in !r

    let string_of_name (L l) = sprintf \"latex_libth_label_%d\" l

    let newtheorem_ star ?opt param =
      let lab = mk_new () in
      L lab,
      unusual_command
        (Printf.sprintf \"newtheorem%s\" (if star then \"*\" else \"\"))
        ((A, brace, text (string_of_name (L lab))) 
         :: (A, brace, param)
         :: match opt with Some t -> [ A, bracket, t ] | None -> []) A

    let newtheorem = newtheorem_ false
    let newtheorem' = newtheorem_ true

    let environment ?packages name ?opt ?args body mode =
      environment ?packages (string_of_name name) ?opt ?args body mode

    let env name body = environment name (A, body) A
  end

  let cite refe = \"cite\" @ ([concat (list_insert "," refe)], A)

  let url x = \"url\" @ ([text x], A)
  let nolinkurl x = \"nolinkurl\" @ ([text x], A)

  type 'a mail = 
    | Mail of 'a
    | Http of 'a

  let href x1 x2 = \"href\" @ ([text (match x1 with Mail x -> \"mailto:\" ^ x | Http x -> \"http://\" ^ x) ; nolinkurl x2], A)

  let http s = href (Http s) s
  let mail s = href (Mail s) s

  let multirow2 x = \"multirow\" @ ([ "2" ; "*" ; x ], A)
  let multicolumn n align x = \"multicolumn\" @ ([ latex_of_int n ; align ; x ], A)

  let latex_of_array_column = function
    | `L -> text \"l\"
    | `C -> text \"c\"
    | `R -> text \"r\"
    | `Vert -> text \"|\"
    | `Raw s -> text \"!\" ^^ within_braces s 
    | `P s -> concat [ "p" ; text \"{\" ; latex_of_size s ; text \"}\" ]
    | `Sep t -> concat [ text \"@{\" ; t ; text \"}\"] 

  type 'a row_line =
    | Data of 'a
    | Hline
    | Cline of int * int

  let hline = \"hline\" @ ([], A)

  let cline i1 i2 = \"cline\" @ ([text (Printf.sprintf \"%d-%d\" i1 i2)], A)

  let tabular x body = 
    environment \"tabular\" ~args:[A, concat (BatList.map latex_of_array_column x)]
      (A, concat (BatList.map (function Data l -> concat (BatList.interleave (text \" & \") l) ^^ newline | Hline -> hline ^^ text \"\n\" | Cline (i1, i2) -> cline i1 i2 ^^ text \"\n\") body)) A

  let bibliographystyle x = \"bibliographystyle\" @ ([x], A)
  let bibliography x = \"bibliography\" @ ([x], A)
  let subparagraph x = \"subparagraph\" @ ([x], A) 
  let footnote () = footnote (* we change the type of Latex.footnote to remember that it has the side effect of resizing fonts *)
  let alltt x = environment \"alltt\" (A, x) A
  let mbox x =  \"mbox\" @ ([x], A)
  let fbox x =  \"fbox\" @ ([x], A)
  let spacing x = 
    environment \"spacing\" ~args:[A, "0.80"]
      (A, x) A
  let longtable x body = 
    space ^^ (* WARNING [longtable] can have a bad behaviour if we delete this [space] *)
    environment \"longtable\" ~opt:(A, "l") ~args:[A, concat (BatList.map latex_of_array_column x)]
      (A, concat (BatList.map (function Data l -> concat (BatList.interleave (text \" & \") l) ^^ newline | Hline -> hline ^^ text \"\n\" | Cline (i1, i2) -> cline i1 i2 ^^ text \"\n\") body)) A
  (*let vdots = \"vdots\" @ ([], A)*)
  let textcircled x = \"textcircled\" @ ([x], A) 
  let ding x = \"ding\" @ ([text (string_of_int x)], A) 

  let upper_important = 
    let open BatSet in
    let l_not = StringSet.of_enum (BatList.enum [ \"with\" ; \"the\" ]) in
    
    BatList.map (fun s -> 
      let esp = \" \" in
      BatIO.to_string (BatList.print ~first:\"\" ~last:\"\" ~sep:esp BatString.print)
        (BatList.map 
           (function 
             | s when s = \"\" || StringSet.mem s l_not -> s
             | s -> BatString.splice s 0 1 (String.make 1 (Char.uppercase s.[0]))) (BatString.nsplit s esp)))

  let concat_line_t l = 
    concat (BatList.interleave newline l)

  let concat_line_string l = concat_line_t (BatList.map (fun s -> text s) l)
  let vrule =  \"vrule\" @ ([], A)
(*
  let fontdimen n = text (Printf.sprintf \"\\fontdimen%d\" n) (*(Printf.sprintf \"fontdimen%d\" n) @ ([], A)*)
  let font = \"font\" @ ([], A)
  let font_ = text \"\\font\"
  let hyphenchar = \"font\" @ ([], A)
  let fontdimen_font n sz = fontdimen n ^^ font_ ^^ text \"=\" ^^ (latex_of_size sz) ^^ text \"\n\"
  let hyphenchar_font_ = text \"\\hyphenchar\\font=`\\-\"  ^^ text \"\n\"

  let justify x = text \"\\justify \" ^^ x
  let justify2 x = text \"\\justify \" ^^ x
(*
  let justify x = 
    List.fold_left (fun acc (n, sz) -> acc ^^ fontdimen_font n sz) ""
    [ 2, `Em 0.4
    ; 3, `Em 0.2
    ; 4, `Em 0.1
    ; 7, `Em 0.1 ]
    ^^ x

  let justify2 x = 
    List.fold_left (fun acc (n, sz) -> acc ^^ fontdimen_font n sz) ""
    [ 2, `Em 0.4
    ; 3, `Em 0.2
    ; 4, `Em 0.1
    ; 7, `Em 0.1 ]
    ^^
      hyphenchar_font_
    ^^ x
*)
*)

  let hypersetup l =  \"hypersetup\" @ ([ concat (BatList.interleave "," (BatList.map (fun (s1, s2) -> s1 ^^ "=" ^^ s2) l)) ], A)
end
open L

let main prelude l = 
  emit
    (document
       ~options:[ `A4paper ; `Pt 11 ] 
       ~packages:(
         BatList.flatten 
           [ [ (* "babel", [ "english"; "francais" ] *)
               "inputenc", "utf8" 
             ; "fontenc", "T1" ]
           ; BatList.map (fun x -> x, "")
             [ "lmodern"
               
             ; "calc" ; "array" ; "alltt" (*; "setspace"*) ; "longtable"
             ; "url"
             
             ; "tikz"
             ; "xcolor"

             (* "xltxtra" *)
             ; "xspace"


             ; "amsmath"
             ; "amssymb"
             ; "amsfonts"
             ; "amsthm"

             (* "latexsym" *)
             (* "graphicx" *)
             (* "multirow" *)

             (* "enumerate" *)
             ; "float"

             (* "bbding" *)
             ; "pifont"

             ; "multirow"

             ; "hyperref" ]
           
           ])
       ~author:(concat_line_t 
                  [ "Frédéric Tuong"
                  ; footnotesize "INRIA - LIAMA"
                  (*; footnotesize (mail \"frederic.tuong@inria.fr\")*) ])
       ~title:(large3 (textbf (concat_line_string (upper_important [ \"fast certified simulation\" ; \" with the SH4 model\" ]))))
       ~date:"November 2010 - October 2011"

       ~prelude:(concat prelude)

       (concat l))

(* \ifhevea\setboolean{footer}{false}\fi *)

(*  \newenvironment{fontsans} *)
(*    {\begin{divstyle}{fontsans}} *)
(*    {\end{divstyle}} *)

type ppp = PPP

type 'a humanc = Comment of 'a list * 'a option

module P = 
struct
  let ocaml = "OCaml"
  let compcert = "CompCert"
  let coq = "Coq"
  let gcc = "GCC"
  let simlight = "simlight"
  let simsoc = "SimSoC"
  let simcert = "SimSoC-Cert"

  let simgen = texttt "simgen"
  let ps = texttt "pseudocode"
  let dec = texttt "decoder"
  let ps_v = ps ^^ texttt ".v"
  let dec_v = dec ^^ texttt ".v"
end

module English =
struct
  let newrelease = "A first public release will be soon available"
  let outworld = "the outside world"

  let coqkernel = 
    let to_check = false in
    let msg = \"The kernel does not recognize yet that a parameter can be instantiated by an inductive type.\" in
    let check () =
      let ic = BatUnix.open_process_in (Latex.to_string
                                          (concat
                                             [ "<<echo '>>"
                                             ; "<<
Module Type MM. 
  Parameter t : Type. 
End MM. 

Module M : MM. 
  Inductive t := . 
End M.
                                                >>"
                                             ; "<<' | coqtop>>" ])) in
      let s = BatIO.read_all ic in
      let _ = BatUnix.close_process_in ic in
      assert (List.length (BatString.nsplit s msg) = 2) in
    let () = if to_check then check () else () in
    "``" ^^ texttt (text msg) ^^ "''"

  let bdiz = beta ^^ delta ^^ iota ^^ zeta

  let yes = \"ding\" @ ([ "51" ], A)
  let no = \"ding\" @ ([ "55" ], A)
  let maybe = "?"
end

module type SIZE_DYN =
sig
  val normal : Latex.t -> Latex.t
  val footnote : Latex.t -> Latex.t
  val tiny : Latex.t -> Latex.t
end

module S_sz (Sz : SIZE_DYN) =
struct
  let mk_ name x = texttt (index "" (Sz.footnote x) ^^ Sz.normal name)

  let mk_sl = mk_ P.simlight
  let gccSL = mk_sl "gcc"
  let cSL = mk_sl "compcert"
  let aSL = mk_sl "asm" (* \overset{\checkmark}{\leadsto} *)
  let lSL = mk_sl "preserved"
  let lambda_leads = exponent lambda (\"leadsto\" @ ([], A))
  let lSL = mk_sl lambda_leads
  let coqSL = texttt "coq{(\"relbar\" @ ([], M)) ^^ P.simlight}"

  let ccasm = texttt "{index "" "compcert"}ASM"
  let mk_C = mk_ "C"
  let compC = mk_C "compcert"
  let gccC = mk_C "gcc"
  let hC = mk_C "human"
  let aC = mk_C "asm"
  (* let lC = mk_C "preserved" *)
  let lC = mk_C lambda_leads
  let mC = mk_C English.maybe

  let mk_Cp = mk_ "[C]"
  let frontC = mk_Cp "frontc"
  let cilC = mk_Cp "cil"
  let cparserC = mk_Cp "cparser"
end

let interl_ f_vert len val_ = BatList.interleave (f_vert `Vert) (BatList.init len (fun _ -> val_))
let interl x = interl_ (fun x -> x) x

module Comment_sz (SZ_s : SIZE_DYN) (SZ_comment : SIZE_DYN) =
struct
  module S = S_sz (SZ_s)
  
  let comment m_beg m_end f_tabular f_col f_vert l o_last =
    let f x = [ multirow2 (SZ_comment.footnote (texttt x)) ] in
    let row = BatList.map f_col [ S.hC ; S.gccC ; S.compC ; S.aC ; if o_last = None then ldots else S.lC ] in
    f_tabular
      SZ_comment.tiny

      (BatList.flatten [ [ `R ] ; interl_ f_vert (List.length row) `C ; [ `L ] ])
      
      (BatList.map (fun l -> 
        Data (BatList.flatten l))
         [ [ f (f_col m_beg) ; row ; f (f_col m_end) ]
         ; BatList.map (BatList.map (fun x -> f_col x)) 
           [ [ "" ] ; l ; [ match o_last with None -> "" | Some last -> last ] ] ])
end

module Comment = 
  Comment_sz
    (struct let normal = scriptsize let footnote = tiny let tiny _ = assert false end)
    (struct let normal = normalsize let footnote = footnotesize let tiny = tiny end)


module Code =
struct

  let num_line f l = 
    longtable [ `Vert ; `C (*`P (`Pt 0.01)*) (*; `Vert*) ; `L ]
      (BatList.flatten
         [ [ Cline (1, 1) ]
         ; BatList.mapi (fun i s -> 
             Data [ (*if i = 0 || i = pred n_end then "${circ}$" else "${vdots}$"*)
                    (*lwavy*)
                    text \" \"
                    (*"${circ}$"*)
                    (*bullet*)
                    (*textit (tiny (latex_of_int (Pervasives.succ i))) *)
                  ; f s ]) l
         ; [ Cline (1, 1) ] ])

  type ('a, 'b) split_result =
    | Text of 'a
    | Delim of 'b


  let output_code = true

  module V = 
  struct
    include Verbatim
    let verbatim = if output_code then verbatim else fun _ -> ""
  end

  module LV =
  struct
    include Latex.Verbatim
    let verbatim = if output_code then verbatim else fun _ -> ""
  end

  let unique_string_of_ppp = 
    let s = Printf.sprintf \"(*%s*)\" (Int64.to_string (Random.int64 Int64.max_int)) in 
    fun PPP -> s

  let latex_of_ppp PPP = textit (Color.textcolor_ Color.green (LV.verbatim \"[...]\"))

  module Raw_ =
  struct
    let verbatim x = 
      concat (BatList.map (function `V s -> text s | _ -> failwith \"to complete !\") x)
  end

  module Raw = 
  struct
    let verbatim x = 
      concat (BatList.map (function
        | `V s -> texttt (LV.verbatim s)
        | `C p -> texttt (latex_of_ppp p)
        | `M m -> failwith \"to complete !\"
        | `T t -> failwith \"to complete !\") x)
  end


  module Coq =
  struct

    module Parse = 
    struct

      let string_of_token = let open Coq_lex in function
        | Comment -> \"Comment\"
        | Keyword -> \"Keyword\"
        | Declaration -> \"Declaration\"
        | ProofDeclaration -> \"ProofDeclaration\"
        | Qed -> \"Qed\"
        | String -> \"String\"

      let parse s =
        let l = ref [] in
        let b = try Some (Coq_lex.delimit_sentence (fun i1 i2 t -> l := (i1, i2, t) :: !l) s) with Coq_lex.Unterminated -> None in
        List.rev !l, 
        b, 
        match !l with
          | (_, p0, _) :: _ -> 
            BatOption.map 
              (fun p1 -> (if s.[p0] = '.' then None else Some (String.sub s p0 (p1 - p0 - 1))), BatString.lchop ~n:p1 s)
              (try Some (Pervasives.succ (BatString.find_from s p0 \".\")) with _ -> None)
          | _ -> None

      let parse_coq s_init =
        let parse s = 
          let l, b, s_end = 
            let rec aux pos acc_l s =
              match parse s with
                | [i1, i2, Coq_lex.Comment], b, s_end -> 
                  let _ = 
                    if (match b with None -> false | Some b -> i2 = Pervasives.succ b)
                    then () else failwith \"to complete !00\" in
                  (match s_end with
                    | None -> [i1, i2, Coq_lex.Comment], None, s_end (*raise Coq_lex.Unterminated*)
                    | Some (Some s, s_end) ->
                      aux (pos + i2) ([ pos + i1, pos + i2, Coq_lex.Comment ] :: acc_l) (Printf.sprintf \"%s.%s\" s s_end))
                | l, b, s_end -> 
                  BatList.flatten (List.rev (BatList.map (fun (i1, i2, t) -> pos + i1, pos + i2, t) l :: acc_l)), BatOption.map ((+) pos) b, s_end in
            aux 0 [] s in
          let l, p = 
            List.fold_left (fun (acc, pos) (i1, i2, t) -> 
              if i1 >= pos then
                (Text (String.sub s i1 (i2 - i1), t) ::
                   (if i1 > pos then
                       Delim (Some (String.sub s pos (i1 - pos))) :: acc
                    else
                       acc)), 
                i2 
              else
                failwith \"overlapping\") ([], 0) l in

          let l, s_end = 
            match s_end with
              | None -> 
                (match b with 
                  | Some b -> 
                    if s = \"\" then failwith \"to complete !1\" else 
                      (Delim (Some (BatString.left s b)) :: Delim None :: l), Some (BatString.lchop ~n:(Pervasives.succ b) s)
                  | None -> 
                    if s = \"\" then failwith \"to complete !2\" else
                      (if 
                          match l with Text (_, Coq_lex.Comment) :: _ -> true | _ -> false
                       then
                          l
                       else
                          let _ = Printf.printf \"iiiiiiiiiiiiiiii%s\n%!\" s in
                          Delim (Some s) :: l), None)
              | Some (s_else, s_end) -> 
                (let _ = if b = None then failwith \"to complete !3\" else () in
                match s_else with 
                  | None -> l
                  | _ -> Delim s_else :: l), Some s_end in
          List.rev l, b, s_end in

        let rec aux acc = function
          | \"\" -> List.rev acc
          | s -> 
            let l, b, o = parse s in
            match o with 
              | None -> List.rev ((l, b) :: acc)
              | Some s_end -> aux ((l, b) :: acc) s_end in
        aux [] s_init

      let batstring_print io s = BatString.print io (Printf.sprintf \"\034%s\034\" (String.escaped s))

      let print (l, b) = 
        Printf.sprintf \"%s, %s\"
          (BatIO.to_string (BatList.print (fun oc d -> 
            BatString.print oc 
              (match d with
                | Text a -> \"Text (\" ^ BatIO.to_string (fun oc (s, t) -> BatString.print oc (Printf.sprintf \"\034%s\034, %s\" (String.escaped s) (string_of_token t)) ) a ^ \")\"
                | Delim o -> \"Delim (\" ^  BatIO.to_string (BatOption.print batstring_print) o ^ \")\"
              )
           )) l)
          (BatIO.to_string (BatOption.print (fun oc i -> BatString.print oc (Printf.sprintf \"%d\" i))) b)

      let print_coq (l : ((string * Coq_lex.token, string option) split_result list * int option) list) = 
        BatIO.to_string (BatList.print (fun io x -> BatString.print io (print x))) l

    end


    let verbatim x = 
      (*let _ = List.iter (function `V s -> Printf.printf \"%s\" s | `C _ -> Printf.printf \"%s\" \"(*[...]*)\") x in*)
      let s = 
        BatIO.to_string (BatList.print ~first:\"\" ~last:\"\" ~sep:\"\" BatString.print)
          (BatList.map (function 
            | `V s -> s
            | `C p -> unique_string_of_ppp p
            | _ -> failwith \"to complete !\") x) in
      let s = LV.trim ['\n'] s in

      let l = Parse.parse_coq s in

      let regroup_endl =
        let rec aux l0 acc = function
          | (Delim None :: l) :: xs -> aux (List.fold_left (fun l0 x -> x :: l0) l0 (Delim None :: l)) acc xs
          | l :: xs -> aux (List.rev l) (List.rev l0 :: acc) xs
          | [] -> List.rev l0 :: acc in
        function
          | [] -> []
          | (Delim None :: _) :: _ -> assert false
          | x :: xs -> List.rev (aux (List.rev x) [] xs) in
      let l = regroup_endl (BatList.map fst l) in

      let l = BatList.map
        (fun l -> 
          let ll =
            BatList.flatten
              (BatList.flatten
                 ([ BatList.map (function 
                   | Text (s, Coq_lex.Comment) when s = unique_string_of_ppp PPP -> [ Text (latex_of_ppp PPP) ]
                   | Text (s, tok) -> 
                       let declaration_color = Color.red (*construct*) (*Color.nonalpha_keyword*) in
                       [ Text (Color.textcolor_ (let open Coq_lex in match tok with
                         | Comment -> Color.comment
                         | Keyword -> (*if s = \"End\" then declaration_color else*) Color.construct
                         | Declaration -> declaration_color (*Color.alpha_keyword*)
                         | ProofDeclaration -> failwith \"to complete !\" Color.violet
                         | Qed -> Color.yellow
                         | String -> Color.string) (LV.verbatim s)) ]
                   | Delim None -> [ Text (Color.textcolor_ Color.black (LV.verbatim \".\")) ]
                   | Delim (Some s) -> 
                     BatList.map
                       (function 
                         | Str.Text s -> Text (LV.verbatim s)
                         | Str.Delim s -> Delim (LV.verbatim s)) 
                       (Str.full_split (Str.regexp_string \"\n\") s)) l
                  ; [[ Text (Color.textcolor_ Color.grey (*"${circ}$"*) (LV.verbatim \".\") (* end sentence *)) ]]])
              ) in
          match l with
            | _ :: Text (\"Notation \", Coq_lex.Keyword) :: _ -> BatList.map (function Text t -> Text (textit t) | x -> x) ll
            | _ -> ll
        ) l in

      let l = BatList.flatten l in
      num_line (fun s -> texttt (footnotesize s)) 
        (BatList.flatten
           (BatList.map (function
             | [] -> []
             | Text _ :: _ as l -> [ concat (BatList.map (function Text x -> x) l) ]
             | Delim _ :: l -> BatList.map (fun _ -> "") l
             | _ -> assert false)
              (BatList.nsplit (function Text _ -> true | Delim _ -> false) l)))

  end
      

  module Ml =
  struct
  
    open Printf

    let stringpair_of_token = function
      | `Comment s -> \"Comment\", s
      | `Construct s -> \"Construct\", s
      | `Keyword s -> \"Keyword\", s
      | `Newline -> \"Newline\", \"\"
      | `String s -> \"String\", s
      | `Quotation s -> \"Quotation\", s
      | `Tab -> \"Tab\", \"\"
      | `Token s -> \"Token\", s
      | `Start_annot (_info, s) -> \"Start_annot\", s
      | `Stop_annot _info -> \"Stop_annot\", \"\"
      | `Special_comment _ -> failwith \"to complete !\"

    let string_of_token x =
      match stringpair_of_token x with
        | a, \"\" -> a
        | a, b -> sprintf \"%s %S\" a b

    let print_tokens (l : Caml2html.Input.token list) =
      List.iter (fun s -> 
        printf \"%s; \" (string_of_token s))
        l

    let verbatim = 
      function
        | [ `V s ] -> 
          let l = Caml2html.Input.string s in
          num_line (fun s -> texttt (footnotesize s)) 
            (BatList.flatten 
               (BatList.map (function
                 | [] -> []
                 | Text _ :: _ as l -> [ concat (BatList.map (function Text x -> x) l) ]
                 | Delim _ :: l -> BatList.map (fun _ -> "") l
                 | _ -> assert false)
                  (BatList.nsplit (function Text _ -> true | Delim _ -> false)
                     (List.map (fun x -> 
                       match
                         match x with
                           | `Comment s -> Text (Some Color.comment, s)
                           | `Keyword s -> 
                             Text (Some
                                     (match s.[0] with
                                       | 'a' .. 'z' -> Color.construct
                                       | _ -> 
                                         (*let _ = 
                                           if String.length s > 1 then
                                             Printf.printf \"%s %!\" (string_of_token (`Keyword s)) in*)
                                         Color.yellow), s)
                           | `Newline -> Delim ()
                           | `String s -> Text (Some Color.string, s)
                           | `Token s -> Text (None, s)
                           | `Construct s -> Text (Some Color.green, s)

                           | `Quotation _
                           | `Tab       
                           | `Start_annot _
                           | `Stop_annot _ 
                           | `Special_comment _ -> failwith \"to complete !\"
                       with
                         | Delim d -> Delim d
                         | Text (color, s) ->
                           Text 
                             ((match color with
                               | None -> fun x -> x
                               | Some c -> Color.textcolor_ c) (LV.verbatim s))
                      ) l))))
        | _ -> failwith \"to complete !\"

  end
  
  module Humc =
  struct
    let verbatim = 
      let f_sz = footnotesize in
      let v col s = Color.textcolor_ col (LV.verbatim s) in
      let v_c c = Str.regexp_string (Printf.sprintf \"%c =\" c), (fun s -> Color.textcolor_ Color.yellow (LV.verbatim (String.make 1 s.[0])) ^^ LV.verbatim \" =\") in
      let v_ty s = Str.regexp_string s, v Color.green in
      let f = fun s -> texttt (f_sz (LV.regexps [ Str.regexp_string \"main\", v Color.construct
                                                            ; v_ty \"int64_t\"
                                                            ; Str.regexp \"<.+>\", v Color.string
                                                            ; v_ty \"int \"
                                                            ; v_ty \"void \"
                                                            ; v_c 'x'
                                                            ; v_c 'y'
                                                            ; v_c 'i'
                                                            ; Str.regexp_string \"_\", v Color.yellow
                                                            ; Str.regexp_string \"return\", v Color.violet
                                                            ; Str.regexp_string \"#include\", v Color.violet
                                                            ; Str.regexp \"\034.+\034\", v Color.string ] (LV.verbatim) s)) in
      function
        | [ `V \"\" ; `C (Comment (l, o)) ; `V x ] -> 
          let l_head, l_body = Comment.comment "/*" "*/" (fun _ x y -> x, y) (fun x -> Color.textcolor_ Color.comment x) (fun _ -> `Raw (Color.color_ Color.comment ^^ vrule)) l o in
          let dim_col = 3 + List.length l in
          longtable (`Vert :: `C (*`P (`Pt 0.01)*) (*; `Vert*) :: l_head)
            (BatList.flatten
               [ [ Cline (1, 1) ]
               ; BatList.map (function Data l -> Data (space :: BatList.map f_sz l) | _ -> assert false) l_body

               ; BatList.mapi (fun i s -> 
                 Data [ space ; multicolumn dim_col "l" (f s) ]) (LV.split_lines (LV.trim ['\n'] x))
               ; [ Cline (1, 1) ] ])

        | _ -> failwith \"to complete !\"
            
  end
end

##verbatim '?' = Code.Raw_.verbatim
##verbatim '!' = Code.Raw.verbatim
##verbatim '#' = Code.Coq.verbatim
##verbatim '~' = Code.Ml.verbatim
##verbatim '@' = Code.Humc.verbatim


module Version =
struct
  let gcc = P.gcc ^^ " 4.5.2"
  let compcert = P.compcert ^^ " 1.9"
  let ocaml = P.ocaml ^^ " 3.12.1"
  let coq = P.coq ^^ " 8.3pl2"
  let coqsvn = "14161"
  let frontc = "3.1#3"
  let cil = "1.3.6#3"
end


module S = S_sz (struct let normal = normalsize let footnote = footnotesize let tiny = tiny end)

module Sfoot = S_sz (struct let normal = footnotesize let footnote = scriptsize let tiny _ = assert false end)

module Label = 
struct
  let simu_sh4, fast_certi, certi_sim, simgendef, oldarm, ctx_compil, concl, appendix_eta, appendix_singl = 
    label (), label (), label (), label (), label (), label (), label (), label (), label ()

  let ex, newth_ex = Th.newtheorem "Example" ~opt:"subsection"
  let note, newth_note = Th.newtheorem' "Notation"
  let fact, newth_fact = Th.newtheorem "Fact"
end


let _ = 
  let l = 
[ tableofcontents 


(*****************************************************************************)
; section "Introduction"
(* ************************ french *)
(* La stabilité d'un système embarqué repose sur le bon fonctionnement de ses composants. Pour effectuer des tests complets sur l'ensemble, avant la phase de fabrication matérielle, les concepteurs s'intéressent à avoir des logiciels simulant le système, prenant en entré un binaire exécutable quelconque. Le projet {P.simsoc} vise à mettre à la disposition des développeurs un tel simulateur, ce dernier étant organisé en différents composants modulaires. Parmi les composants d'un système, le processeur est un élément important aussi bien lors de sa fabrication que lors de sa simulation. D'une part les erreurs de conception engendrent un coût élevé, et d'autre part la durée d'un programme est liée aux instructions du processeur. Pour obtenir une machine sûre, il y a donc une nécessité de travailler avec les simulateurs. Néanmoins, comment garantir qu'un simulateur se comporte {emph "exactement"} comme une machine ? *)
(* Le but de {P.simcert} est justement de répondre à cette question, en proposant de certifier chacune des parties de {P.simsoc}. S'agissant d'un projet ambitieux et d'envergure, en commençant par la correction du processeur, nous nous concentrons en premier sur le c{oe}ur du simulateur.  *)

(* Plus particulièrement, {P.simcert} contient un simulateur du processeur ARMv6 écrit en Coq. Pour le modéliser et l'intégrer au sein de Coq, il a été nécessaire de construire un modèle formel du processeur, ce modèle nous informe précisément sur la sémantique de chaque instruction.  *)
(* Notre travail s'intéresse à l'importation d'un autre type de processeur au projet {P.simcert} : il s'agit du processeur SH4. Comme pour l'ARM, nous montrons à la section [?] comment obtenir un modèle mathématique du SH4 à partir de son manuel de référence. *)
(* ************************ *)
; "Stability of embedded systems depends on the good behavior of their components. Before reaching the construction of the final material in factory, exhaustive testing is thus a strong requirement. At the same time, designer tends to be interested in software simulating the global system and taking a real binary as input. The goal of the {P.simsoc} project~{cite ["ossc09"]} is to permit engineers to develop systems with such a tool, it contains a simulator split modularly into several components. Among these components, the processor is an important element to consider not only during the production but also for the simulation. On one side, errors of conception are susceptible to have a high economical impact. On the other side, the time execution of an algorithm depends on the processor's optimizations. To get a safe system, the need to work with simulators are then going increasingly. However, how can we guarantee the {emph "exact"} behavior between simulator and embedded systems ? 
The goal of {P.simcert} is precisely to answer this question, we propose to certify each part of {P.simsoc}. Being an ambitious project, we plan to begin with the correction of the processor, this targets directly the heart of the simulator.

In particular, {P.simcert} contains a simulator of the ARMv6 written in Coq~{cite ["arm6refman"; "arm"]}. The language Coq is a system which builds a constructive proof from a specification by looking at the programmer's hints~{cite ["Coq:manual"]}. One common application is the extraction procedure which translates the proof, considered as a {lambda}-term, to another functional language like OCaml~{cite ["OCaml"]}. To model and integrate the simulator within Coq, it was necessary to build a formal model of the processor, this model informs us precisely on the semantics of each instruction.
Our work focuses on the importation of another type of processor into the {P.simcert} project : it is the SH4 processor~{cite ["sh4refman"]}. 

As for ARM, we show at section~{ref_ Label.simu_sh4} how to get a mathematical model of SH from his reference manual. Section~{ref_ Label.fast_certi} presents our first step into the certification problem. Our work begins now with some general definitions in section~{ref_ Label.certi_sim}.
"

(*****************************************************************************)
; section ~label:Label.certi_sim "Certified simulation"
; "
This section introduces the concept of software simulation by explaining how simulations work and what informations we need to import from the constructor manual. For the following, we restrict our attention to the simulation of the processor as it is the first and existing component studied in {P.simcert}. We also present the current state of {P.simsoc} and {P.simcert}, and explain at the end the principle and goal of the {P.compcert} compiler.

{Th.env Label.note "
For the following, we will use the symbols
  {itemize
      [ "{S.hC} to designate an arbitrary sequence of character, "
      ; "{S.gccC} for programs compiled successfully with {Version.gcc}~{cite ["gcc"]} (tested at least one time),"
      ; "{S.compC} for programs for which a generated {P.compcert} C file~{cite ["Leroy-Compcert-CACM"]} can be successfully dumped (tested at least one time), in general before the end of the compilation. For a demonstration with {Version.compcert}, see the option <!-dc!>." ]
  }
"}

"
; subsection "Principle of a processor simulator"
(* ************************ french *)
(* Les spécifications du SH4 sont fournies dans un document disponible au format PDF et le fonctionnement de chaque instruction est décrit en langage naturel. Notre but est de construire un modèle mathématique du processeur à partir de cette documentation, un modèle qui soit le plus proche possible de la sémantique informelle fournie par le constructeur. *)

(* Pour l'ARM, un travail similaire a été accompli en extrayant les informations du manuel de référence avec un programme, et après une phase de correction préliminaire sur la syntaxe. *)

(* De manière générale, un simulateur de processeur a essentiellement besoin de deux informations pour fonctionner. *)
(* {itemize *)
(* ; La première est celle décrivant le comportement de chaque instruction, appelé ``pseudocode''. Chaque instruction du processeur (ADD, CPY, MOV ...) est précisément composé d'une séquence de primitives bas niveau : copie ou affectation de registres, opérations d'accès à la mémoire... Parfois, le manuel de référence précise que le comportement n'est pas défini ou incertain dans certaines situations (``unpredictable'' pour l'ARM, ``Appendix B'' pour SH4). *)
(* ; La deuxième information dirige la phase du ``décodage'' des instructions. Étant donné un mot binaire de taille fixe, il s'agit de retrouver à partir de ce mot l'instruction qui lui est associée (pour l'exécuter), et quels sont éventuellement ses arguments. À priori, il pourrait y avoir une ambiguïté sur la recherche d'une instruction correspondant à un mot donné, on pourrait trouver plusieurs instructions candidates pour un mot. L'ordre des instructions à tester en premier a également une importance. Normalement, les indications disponibles dans les manuels sont suffisament clairs pour éviter ces ambiguïtés. *)
(* %Chaque instruction s'accompagnant d'un motif prédéfini dans le manuel, cette recherche s'effectue en les testant un par un jusqu'à l'obtention d'un succès. Cependant, l'ordre des tests peut être important *)
(* %Dans le manuel de référence, ces indications s'obtiennent facilement, car rassemblées sous la forme d'un tableau pour chaque instruction. *)
(* } *)
(* ************************ *)


; "Generally, a processor simulator basically needs two informations in order to accomplish his task.
{itemize
[ "The ``pseudocode'' describes the behavior of each instruction. Each processor instruction (add, cpy, mov ...) are precisely composed of sequences of low-level primitives : copy, affectation of register or access operation to the memory... The unknown or uncertainty behavior in some situations are in general stated in the reference manual (for example, ``UNPREDICTABLE'' case in instruction for the ARM, the same is described in Appendix B for the SH). In any case, specified or not, when importing the semantics to some formal model, we will be forced to explicitly specify the meaning of all the unknown behavior."
; "The ``decoder'' pilots the association from binary word to instruction. Given a binary word of fixed size, the goal is to retrieve from this word the instruction associated to the word, in order to execute it, as well as his potential arguments. 
The correction of the decoder is as important as the pseudocode for a good simulation. We want for example to be sure that there is only one instruction associated to a word." ]
}
Providing these two informations, we are able to model a simple simulator in Coq{let module S = Sfoot in footnote () "Coq being a strongly normalizing language, the simulation is performed within a fixed number of recursive step."}. This is exactly what the {P.simcert} project contains.
"
; subsection "The {P.simcert} project"
; "
The goal of {P.simcert}~{let module S = Sfoot in footnote () "{English.newrelease}~{cite ["urlsscert"]}."} is to certify {P.simsoc}~{cite ["arm"]}, because its large part is written in C++~{cite ["ossc09"]} and currently we are not sure about the safety of our C++ compiler used. However, for the processor simulator, it is possible to abstract most of the surrounding C++ features. We call then {S.gccSL} the {S.gccC} part restricted to the component which simulates the processor. Because we are currently in the same unpredictable situation with a {S.gccC} program, we want to have a formal proof that our {S.gccC} code behaves correctly. But what does ``formal proof'' mean in the case we are comparing a virtual software and a real hardware ? Even if the hardware is produced with the help of the computer, its specification depends directly on the format of the documentation the constructor gives. For our subject of processor certification, the processor documentation is generally available in raw text. For example, specifications of the SH4 are published in a PDF document and inside, the behavior of each instruction are described in natural language. Our goal is to build a mathematical model of the processor from the processor documentation, one which is as closer as possible to the one furnished by the constructor. Then, understanding this definition of ``formal proof'', the {S.gccC} code behind {S.gccSL} needs to behave correctly with respect to the semantic of this mathematical model. Note that to be strict, because the manual can contains some erroneous specifications, further validation tests with the real hardware will remain to be performed in the final end, in order to obtain an observationally equivalent behavior with the simulator.
"
; paragraph "Related work"
; "
A formalization of the ARMv7 processor is reported in~{cite ["conf/itp/FoxM10"]}. The ARMv7 model has been constructed manually using the proof assistant HOL4, with monad as combinator for semantic functions. 
We are interested to automatically generate a Coq model, which aim to be as understandable as possible to the manual of reference. We will show later how coercions and notations in Coq are advantages for readability, but we will also notice some limitations of their use for a large proof project. Organizing the project modularly in Coq will also permit us to integrate simultaneously ARMv6 and SH4 in a generic simulator, and gain an easier interaction with {P.compcert}.
"
; subsubsection ~label:Label.simgendef "The simulator {S.coqSL} and the toolkit {P.simgen}"
; "
Now, before considering the SH, let us look at what we already have in {P.simsoc} and {P.simcert}. On one side, there is {S.gccSL}. On the other side, a mathematical model in Coq had automatically been built for the ARM~{cite ["arm"]} and thus a special simulator has especially been created to integrate as well as to test the model : {S.coqSL}.
The behavior of {S.gccSL} and {S.coqSL}, which have both been manually designed, is intentionally the same : for compiling, these two simulators need the ARM manual specification (in their respective language).

Therefore, {P.simgen}, which is also part of {P.simcert}, has especially been created for the importation of the ARM manual.
To illustrate this, we present here the contents of {P.ps_v} and {P.dec_v}, automatically generated by the toolkit.
"
; subsubsection ~label:Label.oldarm "The ARMv6 {P.ps_v}"

; "
This file contains the semantics of all the instructions written in the Coq language. Each instruction from the manual of reference is translated in a similar Coq function, called ``<!..._step!>'' function. 
Furthermore, as for modularity the generation of the {P.dec_v} is currently done in a separate way than the {P.ps_v}, we create an inductive type $inst$ containing exactly a constructor for each ``<!..._step!>'' function. $inst$ will be useful for interoperability between {P.ps_v} and {P.dec_v}. At the end of {P.ps_v}, there is a main function $step$ which role is, given a value of type $inst$, to retrieve the corresponding ``<!..._step!>'' function to run it.

Instructions from the ARMv6 are a bit special. Given a fixed sized word, we usually think about the instruction corresponding to this word, in particular during the decoding phase. But in ARMv6, we need to parameterized our thought one step higher. Indeed, some instructions take as argument a special parameter. This parameter is intended to be specially treated and a preliminary function need to be apply before the real execution of the instruction. This typical case is the {emph "addressing mode"}.

Because the ARM manual contains roughly 150~instructions (and about 75 special THUMB instructions), we will sketch an example with just 2 instructions : <!A4.1.21 LDM (2)!> and <!A4.1.81 SMLSLD!>. Without entering deeply in details, the reader is invited to have a look at the Coq code (more details in {cite ["urlsscert"]}) or to jump directly to the next section.
"
; paragraph "The addressing mode case"
; subparagraph "Type"
; "
According to the ARM manual, there are 5 special modes.
<#
Inductive mode1 : Type := #{PPP}#.
Inductive mode2 : Type := #{PPP}#.
Inductive mode3 : Type := #{PPP}#.
Inductive mode4 : Type :=
  | M4_Incr_after         #{PPP}# 
  | M4_Incr_before        #{PPP}#
  | M4_Decr_after         #{PPP}#
  | M4_Decr_before        #{PPP}#.
Inductive mode5 : Type := #{PPP}#.
#>(to simplify, we have replaced some non-relevant informations by ``<!!{PPP}!!>'')
"
; subparagraph "Definition"
; "
For each constructor regrouped above, an executing function associated is furnished.
<#
(* A5.4.2 Load and Store Multiple - Increment after *)
Definition M4_Incr_after_step s0 W cond n r #{PPP}# : result * word * word :=
  let start_address := (reg_content s0 n) in
  let end_address := (sub #{PPP}#) in
  let r := block (
    (fun loc b st => if_then #{PPP}#
      (fun loc b st => set_reg n (add #{PPP}#) loc b st) loc b st) ::
    nil) nil true s0 in
    (r, start_address, end_address).
#>
"
; subparagraph "Correspondence between the type and definition"
; "
Finally, this simple part illustrates the symmetry between type and definitions. 
<#
Definition mode1_step (s0 : state) (m : mode1) := #{PPP}#.
Definition mode2_step (s0 : state) (m : mode2) := #{PPP}#.
Definition mode3_step (s0 : state) (m : mode3) := #{PPP}#.
Definition mode4_step (s0 : state) (m : mode4) :=
  match m with
    | M4_Incr_after W c n r => M4_Incr_after_step s0 W c n r
    | M4_Incr_before W c n r => M4_Incr_before_step s0 W c n r
    | M4_Decr_after W c n r => M4_Decr_after_step s0 W c n r
    | M4_Decr_before W c n r => M4_Decr_before_step s0 W c n r
  end.
Definition mode5_step (s0 : state) (m : mode5) := #{PPP}#.
#>
Note that along the pattern matching, the state $s0$ is always present at the right hand side. Even if semantically we understand this as a monadic construction~{cite ["peyton-jones-wadler-93" ; "peyton-jones-tackling-09"]}, we will see in the SH part how to rewrite the whole to explicitly hide $s0$.
"
; paragraph "The instruction case"
; "
Here, the same structure as the {emph "addressing mode"} is done for the {emph "instruction"} case, i.e. we define a structure for type, definitions, and at the end, we relate them with a main correspondence function.
"
; subparagraph "Type"
; "
<#
Inductive inst : Type := 
#{PPP}#
  | LDM2 (m_ : mode4) (cond : opcode) (register_list : word) 
#{PPP}#
  | SMLSLD (X : bool) (cond : opcode) 
      (dHi : regnum) (dLo : regnum) (m : regnum) (s : regnum)
#{PPP}#.
#>
Notice the additional parameter {texttt "m_"} of {texttt "LDM2"}, which contains the specification of the addressing mode.
"
; subparagraph "Definitions"
; "
<#
(* A4.1.21 LDM (2) *)
Definition LDM2_step (s0 : state) cond r s #{PPP}# : result :=
  if_then (ConditionPassed s0 cond)
    (fun loc b st => block (
      (fun loc b st => update_loc n0 (*address*) s loc b st) ::
      (fun loc b st => loop 0 n14 (fun i => 
        if_then (zeq (r[i]) 1)
          (fun loc b st => block (
            (fun loc b st => #{PPP}# (read st (#{PPP}# loc) Word) loc b st) ::
            (fun loc b st => #{PPP}# (add (#{PPP}# loc) (repr 4)) loc b st) ::
            nil) loc b st)) loc b st) ::
      nil) loc b st) nil true s0.
#{PPP}#
(* A4.1.81 SMLSLD *)
Definition SMLSLD_step (s0 : state) X cond dHi dLo m s #{PPP}# : result :=
  if_then (ConditionPassed s0 cond)
    (fun loc b st => block (
      (fun loc b st => update_loc n1 (*operand2*) 
        (if zeq X 1 then Rotate_Right #{PPP}# else reg_content s0 s) loc b st) ::
      (fun loc b st => update_loc64 n0 (*accvalue*) 
        (or64 (#{PPP}# ((*ZeroExtend*)(reg_content st dHi))) #{PPP}#) loc b st) ::
      (fun loc b st => update_loc n2 (*product1*) 
        (mul #{PPP}# (get_signed_half0 (get_loc n1 (*operand2*) loc))) loc b st) ::
      (fun loc b st => update_loc n3 (*product2*) 
        (mul #{PPP}# (get_signed_half1 (get_loc n1 (*operand2*) loc))) loc b st) ::
      (fun loc b st => update_loc64 n4 (*result*) (sub64 (add64 
          (get_loc64 n0 (*accvalue*) loc) 
          (get_loc n2 (*product1*) loc)) 
        (get_loc n3 (*product2*) loc)) loc b st) ::
      (fun loc b st => set_reg dLo (get_lo (get_loc64 n4 (*result*) loc)) loc b st) ::
      (fun loc b st => #{PPP}# (get_hi (get_loc64 n4 (*result*) loc)) loc b st) ::
      nil) loc b st) nil true s0.
#>
"
; subparagraph "Correspondence between the type and definition"
; "
<#
Definition step (s0 : state) (i : inst) : result :=
  match i with
#{PPP}#
    | LDM2 m_ cond r =>
      match mode4_step s0 m_ with (r, s, end_address) =>
        match r with
          | Ok _ _ s1 => LDM2_step s1 cond r s
          | _ => r
        end
      end
#{PPP}#
    | SMLSLD X cond dHi dLo m s => SMLSLD_step s0 X cond dHi dLo m s
#{PPP}#
  end.
#>
In contrast with {texttt "SMLSLD_step"}, the execution of {texttt "LDM2_step"} is preceded by an extra call to {texttt "mode4_step"}.
"
; subsubsection "The ARMv6 {P.dec_v}"
; "
The structure of this file is rather simple. Given a word, we decompose it into a list of raw bits, then a pattern matching is applied to this list. The bits in each clause comes directly from the manual, as well as the instruction associated to each list.
"
; subsection "The {P.compcert} project"
; "
The goal of {P.compcert} is precisely to transform most of the {S.hC} programs to an assembler program, with a semantic preservation certificate. 

Generally, if the compiler can produce an assembler file from a {S.hC} program, the well behavior at runtime of the assembler produced depends on two facts :
{itemize
[ "how the generated assembler file is interpreted,"
; "and the well behavior of the program at {S.compC} production time (as well as a heuristically{let module S = Sfoot in footnote () "Starting from {S.gccC}, there is currently no Coq proof of semantic preservation to {S.compC}
. " (*, because*)} good translation from {S.hC} to {S.compC})." ]
}
The first point is not very problematic as discussed in~{cite ["Leroy-Compcert-CACM"]} and we will give a short comment later.
"
(*****************************************************************************)

; section ~label:Label.simu_sh4 "Simulation of the SH4"
; "
The integration of SH4 in {P.simcert} follows the same algorithm as ARMv6. The first step is to transform the raw $string$ of the manual into a more structured type. For the ARM, the transformation from the manual to Coq has precisely crossed a quite structured abstract syntax tree{let module S = Sfoot in footnote () "The AST is approximatively formed with 400 OCaml words."}. Because the ARM generation to Coq is done from this AST, we plan to fix it as our target for SH. Consequently, we hope the algorithm generating to Coq can be reused rapidly for the SH.

{hspace (`Ex 1.)}

Besides the target fixed at this advanced AST, we show at the next part how the parsing of the SH manual is performed, as well as another intermediate type which is finally used before getting this AST.
"
; subsection "Parsing of the SH4 manual"
; "
(* ************************ french *)
(* Le manuel SH4 contient au total environ 450 pages, la partie où se trouve les informations correspondant au pseudocode et au décodeur occupe une place assez importante, près de la moitié du fichier. Construire directement à la main un modèle en Coq est donc long ou avec risques possibles d'erreurs. De plus, les informations à importer sont à première vue organisées de façon régulière, et il semble donc accessible de les traiter automatiquement avec un programme. *)
(* ************************ *)
The SH manual totals about 450 pages, informations corresponding to the pseudocode and decoder occupies an important part, approximately the half of these pages. Building directly a model at hand in Coq is thus long or with a non negligible risk of errors. 
Furthermore, while reading it briefly, we have been having a strong conjecture that the {S.hC} code specified in the instructions can be easily translated to {S.gccC}. To experiment our intuition, we have planned to use the FrontC package as type representing the {S.gccC} in OCaml.
{Th.env Label.note "
Let us name
{itemize
[ "{S.frontC} for programs parsed successfully with the OCaml package {texttt "FrontC"} {Version.frontc} (tested at least one time). For a demonstration, check if the result of the function <!Frontc.parse_file!> is ``<!PARSING_OK _!>''." ]}
"}
Then, starting from the manual considered as a string, the first step was to write the smallest patch possible in order to get a first well-typed {S.frontC} AST in OCaml (surrounded by the other extra informations, such as for the decoder...).

The parsing of the decoder informations was merely fast because for each instructions, they are clearly specified in an array. We explains now the process done for the instructions.

"
; paragraph "Patching phase"
; "

Interesting informations in the SH manual are located at ``section 9, Instruction Descriptions''. It is formed by a preliminary header containing functions specific to floating instructions, followed by a sequence of description of the instructions. 

Corrections needed to bring to the manual are essentially composed of missing symbol ``;'', missing type informations before some parameters and unbounded value encountered. 

For example, the function {texttt "LDCRn_BANK"} in the part ``9.50, LDC'' uses the unbound variable {texttt "Rn_BANK"}.  After a search, we finally found that it is a meta abbreviation for {texttt "R"}$n${texttt "_BANK"}, with $n {in_} [|0;7|]$, as described in the function's comment. Then this special expansion of $n$ has been specially handled by our SH4 parser.

    "
; paragraph "More confidence by type-checking"
; "
Besides the obtaining of the {S.frontC} AST, we think that all the {S.hC} code behind each instructions are predestined to be ultimately executed. Hence, we can extend our test protocol by compiling them with {P.compcert}.
In fact, the program behind the {P.compcert} parsing step is taken from the CIL library (with some minor modifications)~{cite ["necula"]}. The CIL library includes itself a modified version of the {S.frontC} files. Because it was mainly designed as a tool for analyzing the safety of programs, it is then interesting to observe its parsing result.
{Th.env Label.note "
We introduce
{itemize
[ "{S.cilC} for programs parsed successfully with the OCaml package {texttt "cil"} {Version.cil} (tested at least one time). For a demonstration, check if the result of the function <!Frontc.parse!> returns a value of type <!file!> without raising an exception."
(*; "{S.cparserC} for programs parsed successfully with the {Version.compcert} parsing step (tested at least one time). For a demonstration, check if the result of the function <!Cparser.Parser.file!> returns a value of type <!definition list!> without raising an exception."*) ]
}
"}

The typing process from the CIL AST to {S.compC} has actually permitted us to correct new errors.
{itemize
[ "We have discovered some new misleading types (for instance we introduce manually <!bool_of_word!> and <!nat_of_word!> in the definition of <!FPSCR_PR!> and <!FPSCR_FR!>)."
; "It becomes mandatory to specify the type annotation of undefined functions (e.g. the 9.91 SLEEP instruction use the unbound <!Sleep_standby!> function name)."
; "The order of function declarations has an importance : like OCaml, permutations are not allowed for functions not specified as recursive. This problem has been resolved by rearranging the order at top-level of SH functions, in general a simple reversing suffices." ]
(*paragraph "existe-t-il des modifications pour ce cas : pas de 'return' dans le cas de type fonction renvoyant 'void' ? spécifique à simlight ?"*)
}


"
; subsection "Grouping SH4 and ARMv6 into a common pseudocode generator"
; "
The next step after the obtaining of a ML value representing the SH4 manual is its integration in {P.simcert}. To this end, we have performed two modifications : 
{itemize
[ "at the {S.coqSL} source by adding the model of the SH4 processor (model of the state, particular numbering of register, semantics of remaining undefined functions...),"
; "at {P.simgen} to generate the Coq SH4 manual in the same way as what is done for ARMv6." ]
}
The goal was not only to accomplish these tasks but also to fusion the changes with ARMv6 using modules and functors, to permit future integration of processors.

{hspace (`Ex 1.)}

Generating the Coq code was rather long but easy, because a lot of factorization was necessary. We give briefly the modifications required in the AST.
{itemize
[ "Unlike the ARM, the parameter of each SH instructions are clearly specified in the pseudocode. We then modified the internal AST to support these argument informations."
; "In term of semantics, there is a specific constructor, namely <!return!>, presents in the SH pseudocode. It has been represented in the semantic of the Coq code using monadic form."
; "The default case for the <!switch!> option is also present in several SH instructions, then the necessary changes in the AST has been done." ]
}
    "
; subsubsection "{lambda}-optimization of the Coq manual"
; "
In section~{ref_ Label.oldarm}, we have seen that the body of the function <!LDM2_step!> and <!SMLSLD_step!> are explicitly informative. Indeed, the words <!fun loc b st!> are for example repeated a lot. However, the purpose of the variable <!st!> is to carry the transformation on the state, acting like an accumulator. <!b!> and <!loc!> are also accumulators but we remark that <!b!> is just used at a higher function call level, not inside each instruction. So we want to abstract all of them now. Initially, the code present for the ARM was designed and really thought using monadic specifications, but the current shape is currently not entirely satisfactory. Then we think instead, to simplify these accumulators by hiding them at the cost of rearranging the arguments of several functions, in particular by putting all the accumulators at the end.

Additionally, we also notice that the <!loc!> variable, used to model some local stack for variable, is used frequently even for variable which is affected only once. Indeed, we are attempted to replace them by a native Coq ``<!let!>'' construct when needed.

{hspace (`Ex 1.)}

We will explain later that our goal is to prove the well-defined behavior of the Coq simulator, as well as the {S.gccC} simulator. Modifications on the Coq code can thus be considered as superfluous, except for precisely readability of the proof we plan to do. In particular, it is close to some form of compilation to a factorized {lambda}-code. Without going formally, we are interested to minimize, as possible as it can be, the number of node representing the Coq AST for simplifying the future proof (for example, among the optimizations cited above, one can imagine transforming a 
{texttt ("if {Code.latex_of_ppp PPP} then f f_true else f f_false")} into a {texttt ("f (if {Code.latex_of_ppp PPP} then f_true else f_false)") }).

{hspace (`Ex 1.)}

Here comes the file obtained after performing some simplifications and rewriting to monadic style. We present the ARMv6 file rather than the SH4 because its manual contains rich examples. In any case, modifications affect both outputs in the same manner (for more details, the reader is invited to see both generated files~{cite ["urlsscert"]}).
"
; paragraph "The new ARMv6 {P.ps_v}"
; "
Simplifications are done inside the semantic definition and the correspondence type-definition, in particular declaration for types remains the same.

{hspace (`Ex 1.)}

We assume below 
{itemize
[ "having defined a notation for vectors as
<#
  Notation "{{ a ; .. ; b }}" := 
    (Vcons _ a _ .. (Vcons _ b _ (Vnil _)) ..).
#>
(see the Coq library <!Bvector!>). In particular, we rely on the type-checker to guess the <!nat!> associated to each element."
; "having these notations for semantic functions
<#
  Notation "'<.' loc '.>' A" := (_get_loc (fun loc => A)) 
    (at level 200, A at level 100, loc ident).
  Notation "'<' st '>' A" := (_get_st (fun st => A)) 
    (at level 200, A at level 100, st ident).
  Notation "'<:' loc st ':>' A" := (<.loc.> <st> A) 
    (at level 200, A at level 100, loc ident, st ident).
  Notation "'do_then' A ; B" := (next A B)
    (at level 200, A at level 100, B at level 200).
#>
<!_get_loc!> and <!_get_st!> are two new monadic expressions which give access respectively to the accumulator <!loc!> and <!st!> in their body.
<!next!> is like the <!bind!> operator~{cite ["peyton-jones-tackling-09"]} except that the communicating value is ignored."
; "having a function <!_ret!> which is like the <!return!> combinator~{cite ["peyton-jones-tackling-09"]} except that it also returns <!true!> as a couple with the default value." ]
}
"
; subparagraph "(addressing mode) Definition"
; "
<#
(* A5.4.2 Load and Store Multiple - Increment after *)
Definition M4_Incr_after_step W cond n r #{PPP}# : semfun _ := <s0>
  let start_address := reg_content s0 n in
  let end_address := sub #{PPP}# in
  do_then [ if_then #{PPP}#
       (set_reg n (add #{PPP}#)) ];
  ret_ {{ start_address ; end_address }}.
#>
Intuitively, each ``mode <!..._step!>'' function returns a monadic vector (<!semfun!> being the monadic type) because we want later to perform some uniform treatment for each function and their type need then to be unifiable. This is illustrated below with the introduction of <!mode_step!>.
"
; subparagraph "(addressing mode) Correspondence type-definition"
; "
<#
Definition mode1_step (m : mode1) : _ -> semfun unit := mode_step #{PPP}#.
#>
<!mode_step!> is a kind of initialization function. In particular, the local stack for local variable are initialized to be empty here. <!mode_step!> is precisely used in <!mode1_step!>, <!mode2_step!>, {dots}, <!mode5_step!>.

"
; subparagraph "(instruction) Definitions"
; "
<#
(* A4.1.21 LDM (2) *)
Definition LDM2_step cond r s #{PPP}# : semfun _ := <s0>
  if_then (ConditionPassed s0 cond)
    ([ update_loc n0 (*address*) s
    ; loop 0 n14 (fun i => 
         if_then (zeq (r[i]) 1)
           ([ <:loc st:> #{PPP}# (read st (#{PPP}# loc) Word)
           ; <.loc.> #{PPP}# (add (#{PPP}# loc) (repr 4)) ])) ]).
#{PPP}#
(* A4.1.81 SMLSLD *)
Definition SMLSLD_step X cond dHi dLo m s : semfun _ := <s0>
  if_then (ConditionPassed s0 cond)
    ([ <st> let operand2 := 
              if zeq X 1 then Rotate_Right #{PPP}# else reg_content s0 s in
    let accvalue := or64 (#{PPP}# ((*ZeroExtend*)(reg_content st dHi))) #{PPP}# in
    let product1 := mul #{PPP}# (get_signed_half0 operand2) in
    let product2 := mul #{PPP}# (get_signed_half1 operand2) in
    let result := sub64 (add64 accvalue product1) product2 in
    [ set_reg dLo (get_lo result)
      ; <st> #{PPP}# (get_hi result) ]]).
#>
As for <!M4_Incr_after_step!>, the state parameter <!st!> is encapsulated in the monadic type <!semfun!>. The underscore in the return type <!semfun _!> is present not only to let the type-checker deduce the real answer but also the part corresponding to the generation of this type information in {P.simgen} are merged for <!M4_Incr_after_step!> and all the <!..._step!>. In particular, the type behind the <!_!> is not necessarily the same in the both cases.

In the body, {P.simgen} detects automatically the use of <!st!> or <!loc!>. Hence it adds the corresponding notation when needed.
"
; subparagraph "(instruction) Correspondence type-definition"
; "
<#
Definition step (i : inst) : semfun unit :=
  do_then conjure_up_true;
  match i with
#{PPP}#
    | LDM2 m_ cond r => mode4_step m_ (fun s end_adr => LDM2_step cond r s)
#{PPP}#
    | SMLSLD X cond dHi dLo m s => SMLSLD_step X cond dHi dLo m s
#{PPP}#
  end.
#>
The parameter <!st!> having been moved to the end, we observe a certain symmetry in the shape of correspondence between definition and type. Here, the symmetry is clearly highlighted in all the ``mode <!..._step!>'' function and even in the above <!step!>, except for specific instructions such as <!LDM2!> where a specific treatment for addressing mode remains to be done. However, the <!mode4_step!> call becomes simplified compared to the previous <!step!> in section~{ref_ Label.oldarm}.
"
; subsubsection "Discussions"
; "

"
; paragraph "Limit of the Coq generation"
; "
The packing system of module as a first-class value in {Version.ocaml} encourages us to organize rapidly the code for ARMv6 and SH4. The counterpart is the explicit typing required of the module contents. 

The typing information becomes also mandatory in Coq, when sometimes we need to explicitly redefine by hand some record, especially when {English.coqkernel}. Remark that writing annotations in Coq is more complex than writing annotations in OCaml. For example, we present here a warning where some explicit type annotations need to be manipulated carefully. The datatype modeling the exception in the ARMv6 processor has been defined as follows :
<#
(* ARM *)
Inductive exception : Type :=
  Reset | UndIns | SoftInt | ImpAbort | PFAbort | DataAbort | IRQ | FIQ.
#>
At the SH4 part, we began to copy this definition (with the hope to extend with further constructors later) :
<#
(* SH *)
Inductive exception : Type :=
  UndIns.
#>
However, contrarily as the ARMv6, the OCaml compilation of the extracted SH4 code fails. This is mainly due to a subtlety in the Coq type-checking :
{itemize
[ "<!exception!> has type <!Prop!> in the SH case, whereas it has type <!Set!> in the ARM case. More details about the minimality solution of typing rules of inductive declarations are given in the Coq manual {cite ["Coq:manual"]}. In both cases, ``<!Check exception : Type.!>'' succeeds because of the {index le English.bdiz} convertibility rule."
; "In Coq, it exists modules in which the extraction must completely keep the proofs (like our SH4 problem). In the current {Version.coq}, the extraction wrongly removes the maximum of propositions in modules. This leads to wrong OCaml files and there is no options at extraction time to avoid this." ]
}
The appendix {ref_ Label.appendix_singl} contains more explanations.

{hspace (`Ex 1.)}

We can wonder if the manual we generate can be written in a total monadic style~{cite ["conf/itp/FoxM10"]}. For example, the <!if_then!> constructor can become a <!if_then!> monadic combinator, as well as the several encountered <!ConditionPassed!>. Then we hope one can rewrite {newline}
``<!<s0> if_then (ConditionPassed s0 cond) !{PPP}!!>'' as {newline}
 ``<!if_then (ConditionPassed_s0 cond) !{PPP}!!>'' annihilating the need to use <!_get_loc!> and <!_get_st!>{let module S = Sfoot in footnote () "Because the first call to {texttt "_get_st"} at the start affects the variable {texttt "s0"}, this variable needs to be treated carefully, in a monadic reference for example, if we delete {texttt "_get_st"}."}. However we are in front of a difficulty. Indeed, the automatically generated Coq manual uses extensively the coercion facility, for instance to consider a boolean value as a word (or integer)~{cite ["arm"]}. Hence, the Coq code is finally very close to the original {S.gccC} code from the manual. Because sometimes the <!st!> is deeply embedded in arithmetical expressions, we 
have a problem of coercion at higher-order level. Currently an
expression like
<!zeq 1 true!> is implicitly translated to <!zeq 1 (Z_of_bool true)!>
with <!zeq : Z -> Z -> bool!>.
If now, we have a <!zeq_!> of type <!semfun Z -> semfun Z -> semfun bool!>,
this is not well-typed :
<!zeq_ (ret 1) (ret true)!>,
unless we give explicitly the hint annotation
<!zeq_ (ret (1 : Z)) (ret (true : Z))!>.
Note that for readability we can also introduce a notation and write
something similar to <!zeq_ { 1 } { true }!>.

Because the final goal of the manual is also the proof, we can now wonder if it is finally convenient to perform some proof with notations rather than concrete definitions.
"

; paragraph "Performance"
; "
Currently, the generation of the SH4 manual is complete except for floating and MMU instructions. The Coq ARMv6 manual contains 148 instruction cases (without THUMB) and 204 for the SH4 (by considering also the expanding of the {texttt "R$n$_BANK"}) but the SH {P.ps_v} file size occupies only 2/3 of the one of ARM. The SH output for instructions is very simple because it does not contain precomputation for addressing mode. Consequently, the correspondence between type and definition is completely symmetric, for example we can easily delete the type and replace them by their equivalent ``<!..._step!>'' function.

The compilation of SH {P.dec_v} is long~: about 30s on a recent computer while for the ARM we get only 4s with the same machine. Its time is spent mostly on the pattern-matching for retrieving an instruction given a word, the non redundancy detection may be a possible cause of the long analysis. In particular, by performing some sorting on the clause, we get a better compilation time.

{hspace (`Ex 1.)}

In any case, these results is susceptible to be changed, because we mainly need to be sure that the SH model is correct with respect to the hardware, so further modifications on the model is susceptible to be brought.

"
; paragraph "Correction of the simplification"
; "
Inside each instructions in the manual, we suspect there is at most one access to modify the environment per line. It concerns the storing of value inside memory or to the local stack <!loc!>. We have also encountered several read of mutable variable in one line, which does not seem to conflict with storing at the same time. Because the Coq manual may changed in the future (if we discover some erroneous specifications), the current correctness guarantee relies first on its good type-checking.
Note that for SH4, there is a specific instruction <!Read_Byte!> to access a particular location. We found convenient to set the type of this function to a monadic one, i.e. <!word -> semfun word!> as well as its companion <!Write_Byte!>. Because <!Read_Byte!> can be present several times in a line, it was necessary to coat it with an explicit call to <!bind!>. Then the computation ``sequentially'' continues inside the body of <!bind!>.

About the correction of our simplification process of the accumulator <!st!>, <!loc!> and <!b!>, 
{itemize
[ "we can add more <!_get_loc!> or <!_get_st!> at any positions (accepting at least a well-typed monadic expression), because we already know there is no conflict between these names and other variable names coming from the manual, thanks to the good type-checking of the previous manual. Moreover, we also know that any variable (generally leaving in the {lambda}-calculus theory) is captured by the nearest abstraction."
; "If we add less <!_get_loc!> or <!_get_st!> at any positions (requiring at least one), the semantics can wrongly changed. Because the new manual type-checks correctly for ARM and SH, if for example we add less <!_get_loc!> at a certain position, then it means necessarily that the <!loc!> used refers wrongly to an external <!_get_loc!>." ]
}
Hence, in any case, we hope the test validation of the Coq manual will conclude the discussion. However, by seeing the success of the current validation tests for the ARMv6 (by comparing the output of {S.gccSL} and {S.coqSL}) before the simplification and after, we have a good confidence about its accuracy. We are now going to develop the problem of certification of the simulator, the Coq manual, and its validation.

(*****************************************************************************)
"
; section ~label:Label.fast_certi "Towards a ``fast'' certified simulator"
; "
Recall that our objective is to guarantee the correction of any program simulated by {S.gccSL}, written in ARMv6 or SH4, and the requirement of the same state of registers for {S.gccSL} and {S.coqSL} after each instructions. 

We plan to prove this in Coq, because the sources of {S.coqSL} are already and directly accessible in Coq. Thus, first we need to get a Coq representation of a {S.gccC} code, in particular to be able to work further with the {S.gccSL}. 
Because the {S.coqSL} is Coq well-typed, we already know its termination (after any supposed number of step given). Hence, it just remains to show that its behavior is coherent with respect to {S.gccSL}, that there is some correspondence between {S.coqSL} and {S.gccSL}. Indeed, the equivalence proof we plan to write will contain implicitly the termination property for every instruction simulated by {S.gccSL}, as this last aim to mimic {S.coqSL}. 

Now, for representing the {S.gccSL} code in Coq and reason with, we think about using the {S.compC} type, because we suspect that its {S.gccC} code can be easily transformed to {S.compC}. Above all, the {emph "not wrongly"} behavior (in the sense of~{cite ["Leroy-Compcert-CACM"]}) of {S.gccSL} implies the obtaining of a {emph "not wrongly"} assembler file, observationally equivalent to its source, thanks to the {P.compcert} semantic preservation. The {S.compC} AST is also the first AST in the {P.compcert} translation chain where the {P.compcert} proof begins, and is thus the closest Coq AST to any {S.gccC}.

{hspace (`Ex 1.)}

Globally, we can approximate the Coq ARM/SH manual as a {emph "shallow embedding"} because by definition, instructions in the Coq manual are generated using constructors belonging to the native Coq language. Similarly, the {S.compC} ARM/SH manual obtained from the {S.gccC} source by {P.compcert} is view as a {emph "deep embedding"}, because this time, the information about the instructions are build in Coq with the {S.compC} constructor, the {S.compC} AST having been defined in Coq. The main goal is then to prove the equivalence between the {emph "deep"} and {emph "shallow"} embedding.

We have explained this concept of embedding with the ARM/SH manual, but it is not specially limited to, we plan indeed to generalize to the whole simulator. As the {S.compC} type is defined in Coq, the first task is, given a {S.gccC} file, to find a way to bring up the {S.compC} AST of this {S.gccC} file into Coq. As first application, we could then import the {S.gccC} code of {S.gccSL} into Coq via its {S.compC} AST, and thus we will be ready to start the correspondence proof.

{hspace (`Ex 1.)}

This leads us to the section explaining how such a pretty-printer can be constructed, and more precisely, constructively realized.


"
; subsection "The Coq pretty-printer for {S.compC}"
; "

Our goal is to import the {S.gccC} code in Coq via its {S.compC} AST. We need to work with the value inhabiting the {S.compC} manual, and such, during the development process of the proof. But Coq is a language where the input and output with {English.outworld} can not easily be done in its proper language." (*let module S = Sfoot in footnote () "Enriching Gallina with new tactics implies to modify the source of Coq."*)
; " At the same time, a program well-typed by the Coq type system can be extracted in a more complete programming language to interact easier with {English.outworld}. Because the language chosen for the extraction of {P.compcert} is OCaml, one can modify the sources of the extracted program by adding an OCaml pretty-printer, which target language is Coq ; this in order to get back the value inhabiting the {S.compC} AST of the manual. But to be less dependent of the extracting algorithm (which can perform some renaming on variables), as well as the target language of extraction (which may change in the future), we plan to redact the most achievable part of the pretty printer in Coq. Then, we will be closer to the {S.compC} AST and furthermore, we will have the possibility to evolve in a rich dependent type system framework. 
"
; subsubsection "Example"
; "
In our case, the origin and the target language of the printer are Coq. The type we plan to export{let module S = Sfoot in footnote () "``export'' or ``import'', depending the view we have as the origin language is the target language."}, <!AST.program fundef type!> is formed with a lot of constructors. To illustrate this with a simple example, assume we have defined the following type, inspired from the sources of {P.compcert}~:
<#
Inductive floatsize : Type :=
  | F32 : floatsize
  | F64 : floatsize.

Inductive type : Type :=
  | Tvoid : type
  | Tfloat : floatsize -> type
  | Tfunction : typelist -> type -> type
with typelist : Type :=
  | Tnil : typelist
  | Tcons : type -> typelist -> typelist.

Definition ident := positive.

Record program (A : Type) : Type := mkprogram {
  prog_funct : list (ident * A);
  prog_main : ident
}.

Definition ast := program type.
#>
For each declarations $ty$ above, we would like to have the following printers defined in Coq~:
<#
Check _floatsize : floatsize -> s.
Check _type : type -> s.
Check _typelist : typelist -> s.
Check _ident : ident -> s.
Check _program : forall A, (A -> s) -> program A -> s.
Check _ast : ast -> s.
#>
they transform $ty$ into a simple abstract datatype <!s!>, where <!s!> can be thought as a type similar to the well-known $string$, found in Coq or OCaml.

Dependent types are precisely useful for performing a uniform operation on all the constructor, namely by allowing us to abstract the arity of the constructor we are folding. Indeed, when seeing the declarations above, our first will is to literally copy-paste them to produce these declarations :
<#
Definition _floatsize := __floatsize 
  | "F32" 
  | "F64".

Definition _type_ T (ty : #{PPP}#) := ty _ _floatsize
  | "Tvoid"
  | "Tfloat"
  | "Tfunction"

  | "Tnil"
  | "Tcons".
  Definition _type := _type_ _ (@__type).
  Definition _typelist := _type_ _ (@__typelist).

Definition _ident := _positive.

Definition _program {A} #{PPP}# := @__program #{PPP}#
  {{ "prog_funct" ; "prog_main" }}.

Definition _ast := _program _type.
#>

This can hopefully be done under the condition of having defined <!_INDUCTIVE!> and <!_RECORD!> as~:
<#
  Notation "A ** n" := (A ^^ n --> A) (at level 29) : type_scope.

Check _INDUCTIVE : string -> forall n, s ** n.
  Notation "| x" := (_INDUCTIVE x _) (at level 9).

Check _RECORD : forall n, vector string n -> s ** n.
  Notation "{{ a ; .. ; b }}" := 
    (_RECORD _ (Vcons _ a _ .. (Vcons _ b _ (Vnil _)) ..)).
#>
where the type <!vector!> and ``<!_ ^^  _ --> _!>'' are respectively more deeply explained in the Coq libraries <!Bvector!> and <!NaryFunctions!>.
Note that the function <!_RECORD!> can be implemented using only <!_INDUCTIVE!>. Hence, our combinators of pretty-printing are all simulated by only one, here <!_INDUCTIVE!>~
{let module S = Sfoot in footnote () "In the same spirit, a {texttt "Record"} construction is basically seen as a notation for an {texttt "Inductive"} construction~{cite ["Coq:manual"]}." (* FIXME lien vers la théorie des inductifs plus précis *)
}.

The last ambiguity to resolve is the meaning of <!__floatsize!>, <!__type!>, <!__typelist!> and <!__program!>. In fact, it suffices us to rename them as the function <!floatsize_rect!>, ..., <!program_rect!>, where all the ``<!..._rect!>'' functions are already furnished by Coq everytime a type is defined. However, for mutually recursive defined type such as <!type!> and <!typelist!>, we can use a function which fold completely the structure and also perform the mutually call explicitly (see the <!Scheme!> command for this purpose). This justifies why we have regrouped above the constructors of <!type!> and <!typelist!> together in a unique folding function <!_type_!>.

Finally, as <!_INDUCTIVE!> and <!_RECORD!> return the function type ``<!_ ** _!>'', we need to enhance the type of our recursors <!__floatsize!>, ..., <!__program!> by a more powerful one, and at least {emph "convertible"} to its initial type (the ``convertibility'' relation as defined in~{cite ["Coq:manual"]}). It means that we need to write explicitly their type~:
<#
  Notation "A [ a ; .. ; b ] -> C" := 
    (A ** a -> .. (A ** b -> C) ..) (at level 90).

Definition __floatsize {A} : A [ 0   (* F32       :           _ *)
                               ; 0 ] (* F64       :           _ *)
                             -> _ := #{PPP}#.
Definition _type_ A B (f : _ -> Type) := f (
                             A [ 0   (* Tvoid     :           _ *)
                               ; 1   (* Tfloat    : _ ->      _ *)
                               ; 2   (* Tfunction : _ -> _ -> _ *)

                               ; 0   (* Tnil      :           _ *)
                               ; 2 ] (* Tcons     : _ -> _ -> _ *)
                             -> B).
Definition __type {A} : _type_ A _ (fun ty => _ -> ty) := #{PPP}#.
Definition __typelist {A} : _type_ A _ (fun ty => _ -> ty) := #{PPP}#.
#>
Each number corresponds exactly to the arity attended by the respective constructor.
All the informations present until now are sufficient for the type-checker to automatically deduce the remaining type. With only these declarations, our Coq library of pretty-printing considered as a functor is finished, its instantiation by a module and the definition of <!_INDUCTIVE!> being a secondary task.
"
; subsubsection "Programming discussions"
; "
"
; paragraph "Explicit annotations"
; "
The counterpart of using this kind of simplified way for our printer (i.e. using the dependently form as {newline}
<!| "Tvoid" | "Tfloat" | "Tfunction" | "Tnil" | "Tcons"!>) is highlighted by the necessity to explicitly mention the arity of each constructor. This can constitute a notable inconvenient, but in the absence of annotations, the type reconstruction becomes undecidable.
The other alternative would be to give the constructor as a more normal form ``<!_ -> ... -> _!>''. However, the real <!AST.program fundef type!> contains approximatively a hundred of constructors and specifying the type with a single number has rather been a good compromise.
"
; paragraph "Monadic embedding"
; "
Above, we have described <!s!> as a type similar to $string$. In fact, <!s!> is considered abstractly during the folding of each recursors, except for <!_INDUCTIVE!> which needs to manipulate it. Therefore, we can define it as $"<!s!>" := t~{alpha}$ where $t$ is a monadic type and ${alpha}$ the usual value a monadic type carry with. <!_INDUCTIVE!> being instantiated in a module separated from the pretty-printing functor, the integration remains easy.

Note that as a first consequence, because <!_INDUCTIVE!> is the pretty-printing foundation of every constructor, we can embed inside the monadic type some extra informations, like the current indentation position depending on the depth. Moreover, the basic datatype <!s!>, initially considered as a $string$ can now for example be replaced by a monadic buffer for efficiency.

"
; paragraph "Automation versus maintainability"
; "
The process of creating a raw printing function given a type may be automated and integrated in the Coq sources. However, in the case the value we wish to import is defined with the help of the Gallina language, there may be some difficulty to print exactly the tactics used, as well as the Coq comments <!(* ... *)!>. Hopefully, for our task of importing a {S.gccC} data, this is not a problem because the value is rawly taken from {English.outworld}.

The {S.compC} AST contains a lot of type information at each node (for example, every constructor of <!Csyntax.expr!> carries a field of type <!Csyntax.type!>), so the representation of the value printed is rather expressive. Our actual pretty-printer includes a precomputation algorithm where types are first collected and declared separately with the Coq ``<!Definition!>''.
Before this optimization, the ARM {texttt "slv6_iss.v"} file{let module S = Sfoot in footnote () "It contains the {emph "deep"} embedded manual in Coq."} size were about 52Mo. Now, it is approximatively 2Mo, and the type-checking time is thus clearly improved.

Because natural numbers are also frequently used, it can be more readable to print them using 0-9 digits, instead of their raw inductive constructor definition. Remark that if the number we print is too big, like some position of memory location, OCaml raises a <!Stack_overflow!> with byte-code or <!Segmentation fault!> in native version{let module S = Sfoot in footnote () "In the same spirit, when we try to display a large number with the {Version.coq} pretty-printer, like {texttt "Check 5001."}, if it can print the number, it prints with a warning before."}, because we are in fact calling a not tail recursive function at a too nested level (and it is not specially related to natural numbers). This problem can hopefully be solved by abstracting the printing process for every failing type with a tail recursive function, the whole being instantiated with the extracted code at OCaml side.

With the extraction procedure, we have also encountered an extra {eta}-reduction automatically performed, which gives a not well-typed OCaml code. This is resolved by manually {eta}-expanding the offending term or using a more recent version of Coq where this problem has just been fixed (appendix {ref_ Label.appendix_eta}).

"
; subsection "The creation of {S.aSL}"
; "
We have mentioned at the beginning that one motivation of choosing {S.compC}, as type for our work in Coq, is precisely the {P.compcert} certification proof leading to assembler.

Thus, we are attentive at the result of every compilation steps of {S.gccSL} with {P.compcert}, this includes the success to obtain a {S.compC} code as well as, at the end, an assembly file.

{Th.env Label.note "
For the following, let us introduce
{itemize
    [ "{S.aC} for programs for which a generated assembly file can be successfully dumped (tested at least one time), in general at the end of the compilation. For a demonstration with {Version.compcert}, see the option <!-dasm!>." ]
}
"}

"
; subsubsection ~label:Label.ctx_compil "The context of compilation : 32 vs 64 bits"
; "
By definition {S.gccSL} is GCC well-compiled, but until now, we have not precised the type of the machine used during compilation, e.g. a 32 or 64 bits processor, as well as the type of the processor that the binary will be run on, e.g. again a 32 or 64 bits (this last option can be chosen at the invocation of GCC). Hopefully, after at least four attempts, we found the same success of compilation of {S.gccSL} on any combination of 32 or 64 bits machine, and, 32 or 64 bits processor for the target{let module S = Sfoot in footnote () "Among the bits processor, there are of course others important characteristics describing a computer, for example, are we compiling on/for a PowerPC, IA32 or ARM machine~? Without giving complex details, we just precise that the success of these four attempts has been found on a particular same processor."}.

Like GCC, {P.compcert} also allows us to customize the target of processor for the executable. Unlike GCC, no default choice is provided in the case the target is manually not specified, this choice becomes mandatory in {P.compcert}. By looking at the architecture of our own 32 and 64 bits computers, among the possibilities available by {P.compcert}, we opt to set <!ia32-linux!> first, with the hope to extend to other processors after. But since here, cares must be taken because this simple choice can have a non-trivial influence on proofs. In particular, this {S.gccC} program~:
<@@{let open English in Comment ([ yes ; yes ; no ; no ], None)}@
#include <inttypes.h>

void main() {
  int64_t x = 0;
}
@>
is rejected by {P.compcert} (if we still target the <!ia32-linux!> processor), because 64 bits data-structures are not fully supported yet{let module S = Sfoot in footnote () "with the current {Version.compcert}"}, and this behavior does not depend on the 32 or 64 bits machine {P.compcert} is running.

Hence, a new problem is coming : we realize that the ARM manual uses frequently 64 bits data-structures (as shown in the {texttt "A4.1.81 SMLSLD"} instruction). Then so does the whole {S.gccSL}, which is, precisely at this moment, rejected by {P.compcert}.

"
; subsubsection "Is {S.gccSL} well-typed ?"

; paragraph "Going to {S.compC} to obtain {S.cSL}"
; "
We remarked curiously that the previous {S.gccC} program is {P.compcert} well accepted when the processor target is fixed to <!arm-linux!>, and such, on a 64 bits computer only, not on a 32 bits. It means that the good support for 64 bits data-structures depends on the computer used and the processor target fixed. However, it may be surprising because the big part of the compiler is issued from Coq, a deterministic environment. Where is the error~? In fact, there is not : starting from a {S.gccC} code, the heuristic performed to retrieve a {S.compC} code includes an external call to a preprocessor, namely ``<!gcc -E!>''. As long as it terminates, this call is correct because by definition, the heuristic uses its proper algorithm to transform the most {S.hC} to a {S.compC} program. Like the GCC compiler, the behavior of <!gcc -E!> depends on the options <!-m32!> and <!-m64!>, which targets the processor for the executable. If absent, <!gcc -E!> considers by default the processor currently settled. In fact, in the case of <!ia32-linux!>, the extra option <!-m32!> is present everytime.
Note that for <!arm-linux!>, no options are specified, hence the previously success exhibited earlier on 64 bits is now explained.

"
; paragraph "Going to {S.aC} to obtain {S.aSL}"
; "
Once a {S.compC} value is obtained, it is sure that the compiling process leading to assembler will terminate (this part being from Coq). However in this case, termination is not always a good result. In particular, the chain leading to assembler is written in monadic style. Types checking{let module S = Sfoot in footnote () "We can roughly approximate the Coq conversion from {S.compC} AST to the assembler as a typing process. In particular, it may seem feasible to embed this translation in a dependent type."} are done on the fly during the conversion. When the checks fail, an exception is then returned (as terminating value).

Hopefully, in case of errors, we are clearly informed, i.e. events occur sequentially in this order : a monadic error is returned, no assembly outputs are produced, the {S.compC} value is not useful (if existing and dumped) as well as the Coq value pretty-printed from it, and {S.gccSL} needs to be corrected.
"
; paragraph "Conclusion"
; "
"
; subparagraph "Type-checking"
; "
To force the compilation of {S.gccSL} by {P.compcert} to have a deterministic behavior, one which is independent of the machine used and to obtain an assembly file, which is here a synonym of well consistency checking, we present two possible solutions.
{itemize
[ "``Keep the program {S.gccSL}, Modify the environment {P.compcert}''{newline}
For the <!ia32-linux!> target, inspired from <!arm-linux!>, we tried randomly to change the heuristic from <!-m32!> to the explicit <!-m64!> in the preprocessing stage. Then the compilation successfully terminates ! On 32 and 64 bits computer, we can generate a 32 bits assembly file.
"
; "``Modify the program {S.gccSL}, Keep the environment {P.compcert}''{newline}
Here we replace, in the generated ARM manual as well as the whole {S.gccSL}, every 64 bits type by a record containing two 32 bits field. Usual arithmetical operations on 64 bits are then simulated in 32 bits. 

Because 32 bits data-structures are supported, the compilation process terminates. 
However, contrarily to the previous solution, it requires here to activate in {P.compcert} the emulation of assignment between structs or unions (see the option <!-fstruct-assign!>){let module S = Sfoot in footnote () "Because the activation of this option affects the {P.compcert} heuristic, we can finally wonder if the environment has really been kept !"}." ]
}

Remark that on both solutions, the option <!-fno-longlong!> can be set because ``<!long long!>'' types are not used.

Recall that {S.gccSL} includes initially the generated ARM manual. Because now it compiles correctly with {P.compcert}, it also means that the generated ARM manual is in fact a {S.aC} source.
"
; subparagraph "Validation tests"
; "
After the modification performed above, we now have a single program, called {S.gccSL} or {S.aSL} (depending on the speaking context), compiling with target fixed at 32 and 64 bits. However to be able to think about {S.gccSL} as a more close synonym for {S.aSL}, we need at least to study their behavior at runtime. 

We observed unfortunately that unlike {ref_ Label.ctx_compil}, there exist some tests which fail now. In fact, even if the problem of 64 bits data-structures is resolved at compilation time, some arithmetical operations using 32 bits data-structures can have a not expected behavior at execution time.
More precisely, by examining closely the situation, we remarked for instance that this {S.hC} program~:
<@@{let open English in Comment (BatList.init 4 (fun _ -> yes), None)}@
#include <stdio.h>

void main() {
  int i = 32;
  printf("line 1 %lx\n", 1lu <<  i);
  printf("line 2 %lx\n", 1lu << 32);
}
@>
which in fact belongs to the {S.gccC} and {S.aC} class of programs, has two surprising different behaviors by considering the executables respectively produced by GCC and {P.compcert} (both compiling on/for <!ia32-linux!>). Indeed, we have the following results, depending on the compiler used~:{ newline ^^

texttt (tabular (interl 4 `L)
    [ Data [ "" ; "gcc -m64" ; "gcc -m32 -O0{textrm ","}" ; "gcc -m32 -O{textrm "$n$ ($n {in_} [ {texttt "1"} ; {texttt "2"} ; {texttt "3"} ]$)"}" ]
    ; Data [ "" ; "" ; "{textrm P.compcert}" ; "" (* FIXME can we delete this element ? *) ]
    ; Hline
    ; Data [ "line 1" ; "100000000" ; "1" ; "0" ]
    ; Data [ "line 2" ; "100000000" ; "0" ; "0" ] ]) ^^ newline }

(in OCaml, <!Int32.shift_left 1_l 32!> evaluates to <!1_l!>).

Remark that initially, starting from this {S.hC} code included in {P.simsoc} and {P.simcert}~:
<@@{let open English in Comment (BatList.init 4 (fun _ -> yes), None)}@
#include <stdio.h>

void main() {
  int i = 32;
  printf("line 1 %lx\n", (1lu <<  i) - 1);
  printf("line 2 %lx\n", (1lu << 32) - 1);
}
@>
we wanted to obtain <!ffffffff!> everywhere (this was the previous behavior we had in {ref_ Label.ctx_compil} leading to success on validation tests) and is clearly not the results expected~:{newline ^^

texttt (tabular (interl 3 `L)
    [ Data [ "" ; "gcc -m64{textrm ","}" ; "gcc -m32 -O0{textrm ","}" ]
    ; Data [ "" ; "gcc -m32 -O{textrm "$n$ ($n {in_} [ {texttt "1"} ; {texttt "2"} ; {texttt "3"} ]$)"}" ; "{textrm P.compcert}" ]
    ; Hline
    ; Data [ "line 1" ; "ffffffff" ; "0" ]
    ; Data [ "line 2" ; "ffffffff" ; "ffffffff" ] ]) ^^ newline}

Finally, we have fixed this error to get <!ffffffff!> everywhere : this problem using 32 bits data-structures can be easily avoided by using explicitly the deterministic aforementioned operations on 64 bits data-structures, instead of 32.

Now, validation tests succeed on both {S.gccSL} and {S.aSL}." (* Except this 64 bits data-structures not supported in {P.compcert}, we have not encountered others difficult problems during the compilation.*)
; "
{Th.env Label.fact "
However their possible different behavior at runtime, {S.gccSL}, {S.cSL}, and {S.aSL} come from an initial same source. This source belongs to :
{ let module Comment = Comment_sz
        (struct let normal = normalsize let footnote = footnotesize let tiny = tiny end)
        (struct let normal = normalsize let footnote = large3 let tiny = small end) in
  let open English in Comment.comment "<<{>>" "<<}>>" (fun f_tiny x y -> newline ^^ f_tiny (tabular x y)) (fun x -> x) (fun x -> x) (BatList.init 4 (fun _ -> yes)) None}
"}

"
; subsection "The behavior of {S.aSL}, towards {S.lSL}"
; "
"
; subsubsection "Does {S.aSL} terminate ?"
; "
We are now one step closer to invocate the main {P.compcert} theorem, which predicts completely the behavior of the assembly file produced from {S.aSL}. 
{itemize
[ "Because {S.aSL} has in fact been existentially produced, it means the compilation process has lead to a successful monadic value. This is a condition needed first by the main {P.compcert} theorem. Due to the success to get an assembly file, we conjecture this condition is easy to prove in Coq, in particular using some ``<!vm_compute!>''."
; "Besides that hypothesis, the main {P.compcert} theorem takes precisely as parameter the behavior we estimate for the source. Indeed, it is our task to give a bet on a behavior $b$ the initial {S.aSL} has (at {S.compC} AST production time), and to show that its execution really follows $b$. Then, if $b$ is not classified in particular as a {emph "wrong"} behavior, {P.compcert} will answer us that the assembly executable has surely the behavior $b$." ]
}

Remark that some reorganizations have been done in the last version of {P.compcert}. Before {Version.compcert}, the main theorem of correction needs to take exactly these two hypothesis before proceeding further. In {Version.compcert}, the idea remains the same but more lemmas are provided to help proving that the {S.compC} program will {emph "progress safely"} at each steps. In particular, if we suppose the evaluation being deterministic, analyzing the {S.compC} program within the big-step semantic suffices to deduce the behavior of the assembly. To simply outline our reasoning, we will approximate in the following the ``{emph "progress safety"}'' notion by ``{emph "not wrong"}'' and will not emphasize too much on the evaluation strategy.

{Th.env Label.note "
We will use the abbreviation :
{itemize
[ "{S.lC} for programs which can be successfully transformed to an assembly file with a certified compiler preserving its original semantic (and preserving at least from the {S.compC} big-step semantic). Moreover, the behavior of the initial source is required to be proved {emph "not wrong"}." ]
}
"}

{Th.env Label.ex "
This {S.aC} code is not a {S.lC} program :
<@@{let open English in Comment (BatList.init 4 (fun _ -> yes), Some no)}@
int main(int _) {
  return 0;
}
@>
because the type of the main function (called from {English.outworld}) is not of the form {texttt "unit {rightarrow} int"}. Thus it initially goes wrong by definition.
"}

Due to the supposed Turing-completeness of the ARM programming language, we think that {S.aSL} does not {emph "terminate"} and precisely that it is characterized by the {emph "reactive divergence"} behavior (which at the same time does not belong to the {emph "going wrong"} behavior, as specified in {P.compcert}).

"
; paragraph "The behaving proof on a simple program"
; "
Because {S.aSL} contains a lot of lines, we tried first to prove the termination of this simple {S.aC} program~:
<@@{let open English in Comment (BatList.init 4 (fun _ -> yes), Some maybe)}@
main() {
  int x = 2;
  int y = 3;
  return x + y;
}
@>
in particular by using mainly the <!eapply!> tactic. However, as long as the proof grows up, this tactic takes abnormally a too long time to succeed, in particular in front of a huge term. During that time, we were thinking instead to an other way to solve the behaving problem. This leads to the part explaining the shortcut found.
"
; paragraph "{S.lSL} obtained with a meta consideration on Coq"
; "
The example above shows us that proving the behavior, of an apparently simple program, is susceptible to take a long time. We also remark that this task is finally maybe not a priority, especially in the case we can easier prove first there is some kind of equivalence between this program and a Coq program.

Finally, we can wonder if we can begin first with the equivalence between {S.aSL} and {S.coqSL} (at the {S.compC} value reached time), than trying to obtain a {S.lSL} by betting on a behavior $b$. In fact, with the former proceeding, we would have an equivalence between the deep AST and functions in Coq. It means we will have implicitly the proof of the non wrong-behavior of this deep AST, due to the supposed non wrong-behavior of the Coq system.
Then we will know at the meta level that {S.aSL} can not have a {emph "wrong"} behavior, even if a particular $b$ has not yet been exhibited in Coq at that time, a condition needed for the creation of {S.lSL}. 

In conclusion, if we choose first to prove a kind of equivalence between {S.aSL} and {S.coqSL}, and succeed, we will have meta proved the non wrong behavior for {S.aSL}. 
 By extending the reasoning further, we would be sure that the semantic of the initial source has been preserved to assembler. Moreover, we conjecture all this reasoning can yet be formally proved in a type system not different than at Coq level, by starting to exhibit a particular behavior $b$ from one side of the equivalence : from the {S.aSL}, or maybe the {S.coqSL} side...
" (* On both cases, during the establisment of the two proofs, we are informed if we encounter some not well-formed part in {S.aSL} (part that need to be changed).*)

; subsubsection "Future work : the equivalence proof"
; "
"
; paragraph "Coq {longrightarrow_} {S.lC}~?"
; "
Initially, before the creation of any simulator in {P.simsoc}, remark that to get at the end at least one {S.lC} simulator, we could initially take another approach, that is to start with a complete simulator in Coq (a similar one to {P.simsoc}, not only {S.aSL}), then to modify and equip Coq with a constructive extraction procedure into {S.lC} (like ML, Haskell or Scheme). This solution is feasible, because {S.lC} has a formal semantic (since the {S.compC} AST), and rather general as the extraction process can be applied to any Coq program. However, as the project {P.simsoc} has historically been established before {P.simcert}, the organization of the {S.gccC} code behind {P.simsoc} is currently rather detailed and complex now, compared to the existing one at Coq side. Moreover, in term of efficiency, it seems not trivial how to perform in Coq the optimization necessary for setting good performances in SimSoC. Hence, the extraction from Coq is interesting, but we are also interested to know which large part of this {S.gccC} simulator can automatically be translated in Coq and which can not.
"
; paragraph "Coq {longleftarrow_} {S.lC}~?"
; "
The problem we are focusing is more open than only oriented from Coq to {S.lC}. For example, even if the Coq manual is usually considered as the model of reference, for validation, tests are usually performed in {S.gccSL} and {S.aSL} due to performance issue. Indeed, we are interested in a semantical preservative way to report back modifications from {S.lC} side to Coq. 

More generally, it may be difficult to prove the semantical preservation from a Turing-complete language to Coq. Nevertheless, we conjecture the {S.aC} manual is only formed with recursive functions. If we omit this semantical preservative requirement, the question remains important for proving the correction of an arbitrary {S.lC} code. Given a {S.hC} code, under which conditions can one retrieve a ``similar'' Coq code, attesting its good termination ? 

"
; paragraph "ML {longrightarrow_} (Coq {longleftrightarrow_} {S.lC}) ?"
; "

In~{ref_ Label.simgendef} and the SH part, we have explained the automatic importation of the ARM/SH manual by {P.simgen} into {S.gccC}, and later {S.aC}. By following our reasoning of translating {S.lC} into some form of Coq code, it is legitimate to ask if we can also translate the {S.aC} manual in Coq directly. However, the generation of the {S.aC} manual being an automatic process, we had found convenient to use the existing code to produce the Coq manual in the same OCaml environment. Then, {P.simgen} generates both the {S.aC} manual and the Coq manual. By catching the reasoning in this context, the intention to prove the equivalence between these {emph "two"} outputs of a {emph "single"} program (here {P.simgen}) from a {emph "single"} input is less astonishing. 

For this particular case, the generation of {S.aC} being automatic, instead of proving directly the output's equivalence, we can think about proving the good generation starting from the {P.simgen} AST. Indeed, by approximating the raw Coq source into its AST (the Coq AST), as well as approximating the {S.aC} source into the {S.compC} AST, 
{itemize
[ "on one hand we have an ML function translating from {P.simgen} AST to Coq AST,"
; "on the other hand, we have another ML function from {P.simgen} AST to {S.compC} AST." ]
} 
As we think they can easily be translated in Coq, the problem of good equivalence between the Coq manual and the {S.aC} manual can be simplified to the problem of building a couple given a particular {P.simgen} AST, i.e writing a Coq function of type : {newline}
<!simgen_AST -> { (man_coq, man_C) | man_coq <~> man_C }!>. Of course, the equivalence function ``<!<~>!>'' still remains to be defined, but constructing this function-proof may be easier than working directly on the output. Hence, this solution can figure as a potential candidate to explore.

(*****************************************************************************)
"
; section ~label:Label.concl "Conclusion"
; "

We have obtained a SH4 formal model using the same algorithm as was employed for the ARMv6, by automatic extraction of pseudo-formal descriptions and automatic conversion to Coq syntax. The merge into {P.simcert} was performed modularly with functors in order to facilitate the integration of future processors.

The importation of a {S.compC} value via our Coq pretty-printer being ready, the next step is to prove the equivalence between the existing Coq model and the {S.aC} model. After this, we will extend our proof to the complete part of the processor and system simulator : code specialization, dynamic recompilation on the host machine...

Finally, we hope to plug our certified simulator after the {P.compcert} chain leading to assembler source. In contrast with the bootstrapping process which aim to create a higher language than existing, this would terminate the certifying chain preserving the semantics of high-level software to safe execution on hardware.
"
(*****************************************************************************)
; bibliographystyle "alpha"
; bibliography "t"

(*****************************************************************************)
; newpage
; appendix

; section "Appendix"

; subsection "Errors in the OCaml extracted code"
; "
For the following, starting from a well-typed Coq program, the extraction gives wrong {Version.ocaml} files. This behaviour has been seen with at least the version trunk {Version.coqsvn}~and the {Version.coq} (in the examples, we will present code written in style {Version.coqsvn}).
" (* we are going to submit a bug report after further tests if needed, in particular with the current ``trunk'' development version. *)
; subsubsection ~label:Label.appendix_eta "The optimized {eta}-simplification"
; "
{http \"coq.inria.fr/bugs/show_bug.cgi?id=2570\" ^^ newline}
This extracted implementation from a Coq code~:
<~
open Datatypes
open List
open Vector

type __ = Obj.t

(** val to_list0 : nat -> 'a1 t -> 'a1 list **)

let to_list0 n = function
| Coq_nil -> Datatypes.Coq_nil
| Coq_cons (x, n0, t0) -> Datatypes.Coq_cons (x, Datatypes.Coq_nil)

(** val u1 :
    nat -> (__ -> nat -> __ t -> __ list) -> 'a1 t -> 'a3 list -> 'a2 t ->
    'a1 -> 'a1 **)

let u1 n to_list1 sep xs surr =
  let v_to_list = to_list0 n in
  let c = fun s l -> combine (v_to_list s) l in
  List.fold_left (fun x x0 -> x) (c sep (c surr xs))
~>
is accepted by the OCaml type-checker but it does not match its extracted interface, because <!u1!> has a too general type. This can be resolved by manually replacing <!let v_to_list = to_list0 n!> by <!let v_to_list v = to_list0 n v!>.

"
; paragraph "Others possible solutions"
; "
{itemize
[
"Note that in the original Coq code~:
<#
Require Import Vector.
Require Import List.

Definition to_list0 : forall A n,
  Vector.t A n -> { _ : list A | True } :=
  fun A _ v => @exist (list A) (fun _ => True) 
    match v with 
    | Vector.cons x _ _ => List.cons x List.nil
    | Vector.nil => List.nil
    end I.

Definition u1 : forall M N L n
  (to_list1 : forall A n,
  Vector.t A n -> { _ : list A | True })
  (sep : Vector.t M n)
  (xs : list L)
  (surr : Vector.t N n),
  M -> M.
  intros until 4;
  pose (v_to_list A v := let (l, _) := to_list0 A n v in l);
  pose (c := fun B n s l => @List.combine _ B (v_to_list n s) l);
  refine (List.fold_left _ (c _ _ sep (c _ _ surr xs)));
  auto with *.
Defined.
#>
if we replace, <!to_list0!> by <!to_list1!> in <!u1!>, the extracted implementation matches correctly its interface~:
<~
let u1 n to_list0 sep xs surr =
  let v_to_list = to_list0 __ n in
  let c = fun s l -> combine (v_to_list s) l in
  List.fold_left (fun x x0 -> x)
    (Obj.magic (fun _ _ s l -> c s l) __ __ sep
      (Obj.magic (fun _ _ s l -> c s l) __ __ surr xs))
~>
"
; "
Instead of doing a manual {eta}-expansion, we can disable the optimization responsible of this error in Coq with~:
<#
Set Extraction Flag 494.
#>
(see the file {texttt "coq_svn_{Version.coqsvn}/plugins/extraction/table.ml"} for others possibilities with numbers).
Then, we have this well-accepted code, which does not use <!Obj.magic!>~:
<~
let u1 n to_list1 sep xs surr =
  let v_to_list = fun _ v -> to_list0 n v in
  let c = fun _ _ s l -> combine (v_to_list __ s) l in
  List.fold_left (fun x x0 -> x) (c __ __ sep (c __ __ surr xs))
~>
" ]}

"
; paragraph "Localization in the source"
; "
In {texttt "coq_svn_{Version.coqsvn ^^ Code.latex_of_ppp PPP}mlutil.ml"}, the {eta}-reduction performed in the function <!kill_dummy!> does not perform an {eta}-reduction test to avoid a not generalizable <!'_a!>. At the time of writing, it is not sure that this test may or not be done in a similar way as {newline ^^ texttt "coq_svn_{Version.coqsvn ^^ Code.latex_of_ppp PPP}extraction.ml"}.
Remark also, even if it has not been tested, that we may factorize the part under the case <!MLletin!> in <!kill_dummy!> with the <!MLletin!> part in <!kill_dummy_hd!> by abstracting what is necessary.
"
; subsubsection ~label:Label.appendix_singl "The optimized simplification of proofs on modules"
; "
{http \"coq.inria.fr/bugs/show_bug.cgi?id=843\" ^^ newline}
According to the {index le (English.bdiz)} convertibility rule, an object of type <!Prop!> can be identified as an object of type <!Set!> or <!Type!>. In particular, this is a program well-compiled by Coq :
<#
Inductive F := .

Module Type MT.
  Parameter t : Type.
End MT.

Module M : MT.
  Definition t := F.
End M.
#>
However, the extracted OCaml program is not well-typed because the extracted module <!M!> is empty whereas its module type <!MT!> is not : ``The field <!t!> is required but not provided''.

"
; paragraph "Localization in the source"
; "
{itemize
[
"In {texttt "coq_svn_{Version.coqsvn ^^ Code.latex_of_ppp PPP}extract_env.ml"}, the function <!extract_seb!> calls <!Modops.add_signature!> before <!extract_sfb!> to update the current environment from <!env!> to <!env'!>. However, at the same time, there is also no call to <!Visit.add_ref!> in case we encounter an association $["<!t!>" {leadsto} T] {in_} "<!env!>"$ and an association $["<!t!>" {leadsto} "<!Prop!>"] {in_} "<!env'!>"$, if <!Prop!> ${index le English.bdiz} T$ and the {English.bdiz}-normal form of $T {ne}$ <!Prop!>. Then, <!extract_sfb!> considers that the extraction of <!t!> can be ignored. This extracting omission can generally be performed for a proposition, but our term <!t!> has been recategorized as <!Type!> due to the module type constraint."
;
"Similarly, for the functor case, <!extract_seb!> calls <!Modops.add_module!> without doing the above checking. Indeed, by replacing ``<!Module M : MT.!>'' with ``<!Module M (M : MT) : MT.!>'', we have an empty functor in the extracted source." ]
}

"
; paragraph "Others possible solutions"
; "
{itemize
[
"We can explicitly strengthened the type of <!F!> by writing ``<!Inductive F : Set := .!>''."
; 
"At the end, if we mention that the definition inside the module will be used : ``<!Definition t := M.t.!>'', the extraction works correctly because <!M.t!> has been classified as a value that the extraction can not avoid. Hence, <!Visit.add_ref!> will be called before we enter in <!extract_sfb!>." ]
}
"
; paragraph "Notes"
; "
The proposed solution is enough to get a correct extracted OCaml files for our SH4 project, but is not completely a general form for an arbitrary Coq program. For example, we would have the same problem for a module where the typing of its contents is later than its definition :
<#
Module M.
  Definition t := F.
End M.

Module Make (M : MT).
End Make.

Module MM := Make M.
#>
"]
  in

  let l_hypersetup =
    BatList.flatten 
      [ [ "colorlinks", "true" ]
      ; BatList.map 
        (fun (n, r, g, b) -> n, Color.color_name_ (Color.of_int_255 (r, g, b))) 
        [ "linkcolor", (*137, 59, 187*)144, 0, 24
        ; "citecolor", 0, 163, 100
        ; "urlcolor", 0, 60, 141 ] ] in

  main 
    [ Label.newth_ex
    ; Label.newth_note
    ; Label.newth_fact
    ; Color.definecolor_used (fun c l -> l ^^ Color.definecolor c) ""
    ; hypersetup l_hypersetup ]
    l
