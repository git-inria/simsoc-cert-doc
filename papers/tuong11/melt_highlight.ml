(**
SimSoC-Cert, a toolkit for generating certified processor simulators
See the COPYRIGHTS and LICENSE files.
*)

open Melt_lib open L

open Printf

type ppp = PPP | PPP_full | PPP_empty

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

  let string_of_ppp, ppp_of_string =
    BatList.split
      (BatList.map
         (fun p ->
           let s = sprintf \"(*%s*)\" (Int64.to_string (Random.int64 Int64.max_int)) in
           (p, s), (s, p))
         [ PPP ; PPP_full ; PPP_empty ])

  let latex_of_ppp =
    let rose = Color.of_int_255 (0xFF, 0x4D, 0xE6) in
    function
    | PPP -> textit (Color.textcolor_ Color.green (LV.verbatim \"[...]\"))
    | PPP_full -> Color.textcolor_ rose "{bullet}"
    | PPP_empty -> Color.textcolor_ rose "{circ}"

  module Dot =
  struct

    type shape =
    | Note
    | Box of bool (* true: rounded *)

    type node =
      { n_color : string
      ; shape : shape }

    type edge =
      { e_color : Latex.t option
      ; style : string option }

    type header =
      { shift_x : float
      ; shift_y : float
      ; scale : float option
      ; node : node
      ; edge : edge }

    type antiquote =
    | Header of header
    | B_escape_nl of Latex.t
    | B of Latex.t

    let l_to_string s =
      BatString.map (function '\n' -> ' ' | x -> x) (Latex.to_string s)

    let verbatim x =
      let () = ignore (List.iter (function `V _ | `C _ -> () | _ -> failwith \"to complete !\") x) in

      let opt, f_after, x =
        match x with
        | `V \"\" :: `C (Header header) :: x ->
          \"--codeonly\",
          tikzpicture
            ~shift:(header.shift_x, header.shift_y)
            (text
               (sprintf \"overlay, shift={(current page.south west)}%s\"
                   (match header.scale with
                   | None -> \"\"
                   | Some scale -> sprintf \", scale=%f, every node/.style={transform shape}\" scale))),
          [ [ `V (BatString.concat \"\n\" [ \"digraph G {\"
                                          ; \"  margin=0\"
                                          ; \"  compound=true\"
                                          ; \"  ranksep=0.01\"
                                          ; \"  nodesep=0.1\"
                                          ; sprintf \"  node [height=0.1, color=%s, shape=%s]\"
                                              header.node.n_color
                                              (match header.node.shape with
                                              | Note -> \"note\"
                                              | Box b -> \"box\" ^ if b then \", style=rounded\" else \"\")
                                          ; if (header.edge.style, header.edge.e_color) = (None, None) then
                                              \"\"
                                            else
                                              sprintf \"  edge [%s%s]\"
                                                (match header.edge.style with None -> \"\" | Some s -> sprintf \"style=%S\" s)
                                                (match header.edge.e_color with None -> \"\" | Some s -> sprintf \", color=%s\" (Latex.to_string s))
                                          ] ) ]
          ; x
          ; [ `V \" }\" ] ]
        | `V \"\" :: `C (B_escape_nl _ | B _) :: x -> assert false
        | _ ->
          \"--figonly\", (fun x -> x), [x] in

      let ic, oc = BatUnix.open_process (sprintf \"dot2tex %s --autosize\" opt) in
      let () =
        begin
          List.iter
            (List.iter
               (fun o ->
                 BatInnerIO.nwrite oc
                   (match o with
                   | `V s -> s
                   | `C (Header _) -> assert false
                   | `C (B_escape_nl s) -> l_to_string s
                   | `C (B s) -> Latex.to_string s
                   | _ -> failwith \"to complete !\")))
            x;
          BatInnerIO.close_out oc;
        end in

      f_after (text (BatInnerIO.read_all ic))

    let color_name_ c = B (Color.color_name_ c)
    let multiline_ x = B_escape_nl (multiline_ x)
    let textcolor_ c s = B (Color.textcolor_  c s)
    let multiline x = B_escape_nl (multiline x)
    let symbolc x = B (symbolc x)
  end

  module Raw_ =
  struct
    open Dot

    let verbatim x =
      concat (BatList.map (function
      | `V s -> text s
      | `C (Header _) -> assert false
      | `C (B l) -> l
      | `C (B_escape_nl l) -> text (l_to_string l)
      | _ -> failwith \"to complete !\") x)
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

  module Raw__ =
  struct
    open Dot

    let verbatim =
      function
        | [ `V s ] ->
          let l = BatString.nsplit s ~by:\"\n\" in
          num_line (fun s -> texttt (footnotesize s))
            (BatList.flatten
               (BatList.map (function
                 | [] -> []
                 | Text _ :: _ as l -> [ concat (BatList.map (function Text x -> x) l) ]
                 | Delim _ :: l -> BatList.map (fun _ -> "") l
                 | _ -> assert false)
                  (BatList.nsplit (function Text _ -> true | Delim _ -> false)
                     (BatList.interleave (Delim ()) (BatList.map (fun x -> Text (LV.verbatim x)) l)))))
        | _ -> failwith \"to complete !\"
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
                      aux (pos + i2) ([ pos + i1, pos + i2, Coq_lex.Comment ] :: acc_l) (sprintf \"%s.%s\" s s_end))
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

      let batstring_print io s = BatString.print io (sprintf \"\034%s\034\" (String.escaped s))

      let print (l, b) =
        sprintf \"%s, %s\"
          (BatIO.to_string (BatList.print (fun oc d ->
            BatString.print oc
              (match d with
                | Text a -> \"Text (\" ^ BatIO.to_string (fun oc (s, t) -> BatString.print oc (sprintf \"\034%s\034, %s\" (String.escaped s) (string_of_token t)) ) a ^ \")\"
                | Delim o -> \"Delim (\" ^  BatIO.to_string (BatOption.print batstring_print) o ^ \")\"
              )
           )) l)
          (BatIO.to_string (BatOption.print (fun oc i -> BatString.print oc (sprintf \"%d\" i))) b)

      let print_coq (l : ((string * Coq_lex.token, string option) split_result list * int option) list) =
        BatIO.to_string (BatList.print (fun io x -> BatString.print io (print x))) l

    end


    let verbatim x =
      (*let _ = List.iter (function `V s -> Printf.printf \"%s\" s | `C _ -> Printf.printf \"%s\" \"(*[...]*)\") x in*)
      let s =
        BatIO.to_string (BatList.print ~first:\"\" ~last:\"\" ~sep:\"\" BatString.print)
          (BatList.map (function
            | `V s -> s
            | `C p -> List.assoc p string_of_ppp
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
          | (Delim None :: x) :: xs
          | x :: xs -> List.rev (aux (List.rev x) [] xs) in
      let l = regroup_endl (BatList.map fst l) in

      let l = BatList.map
        (fun l ->
          let ll =
            BatList.flatten
              (BatList.flatten
                 ([ BatList.map (function
                   | Text (s, Coq_lex.Comment) when List.exists (fun (s0, _) -> s = s0) ppp_of_string -> [ Text (latex_of_ppp (List.assoc s ppp_of_string)) ]
                   | Text (s, tok) ->
                       let declaration_color = Color.red (*construct*) (*Color.nonalpha_keyword*) in
                       [ Text (Color.textcolor_ (let open Coq_lex in match tok with
                         | Comment -> Color.comment
                         | Keyword -> (*if s = \"End\" then declaration_color else*) Color.construct
                         | Declaration -> declaration_color (*Color.alpha_keyword*)
                         | ProofDeclaration -> Color.violet
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


  module type HUMANC =
  sig
    type t

    val comment :
      Latex.t ->
      Latex.t ->
      ((Latex.t -> Latex.t) -> ([> `C | `L | `R ] as 'a) list -> Latex.t list Melt_lib.L.row_line list -> 'b) ->
      (Latex.t -> Latex.t) ->
      ([> `Vert ] -> 'a) ->
      t ->
      'b

    val length : t -> int
  end

  module Humc (H : HUMANC) =
  struct
    let verbatim =
      let f_sz = footnotesize in
      let v col s = Color.textcolor_ col (LV.verbatim s) in
      let v_c c = Str.regexp_string (sprintf \"%c =\" c), (fun s -> Color.textcolor_ Color.yellow (LV.verbatim (String.make 1 s.[0])) ^^ LV.verbatim \" =\") in
      let v_ty s = Str.regexp_string s, v Color.green in
      let f = fun s -> texttt (f_sz (LV.regexps [ Str.regexp_string \"main\", v Color.construct
                                                            ; v_ty \"int64_t\"
                                                            ; Str.regexp \"<.+>\", v Color.string
                                                            ; v_ty \"int \"
                                                            ; v_ty \"void \"
                                                            ; v_ty \"unsigned long\"
                                                            ; v_ty \"S\"
                                                            ; v_c 'x'
                                                            ; v_c 'y'
                                                            ; v_c 'i'
                                                            ; Str.regexp_string \"_\", v Color.yellow
                                                            ; Str.regexp_string \"struct\", v Color.violet
                                                            ; Str.regexp_string \"return\", v Color.violet
                                                            ; Str.regexp_string \"#include\", v Color.violet
                                                            ; Str.regexp \"\034.+\034\", v Color.string ] (LV.verbatim) s)) in
      function
        | [ `V \"\" ; `C l ; `V x ] ->
          let l_head, l_body = H.comment "/*" "*/" (fun _ x y -> x, y) (fun x -> Color.textcolor_ Color.comment x) (fun _ -> `Raw (Color.color_ Color.comment ^^ vrule)) l in
          let dim_col = 2 + H.length l in
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
