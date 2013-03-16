open Melt_lib open L

open Printf

type ppp = PPP

type 'a humanc = Comment of 'a list

module P =
struct
  let ocaml = "OCaml"
  let compcert = "CompCert"
  let coq = "Coq"
  let gcc = "GCC"
  let simlight = "simlight"
  let simlight2 = "simlight-opt"
  let simsoc = "SimSoC"
  let simcert = "SimSoC-Cert"

  let simgen = "simgen"
  let manual = "manual"
  let pseudocode = "pseudocode"
  let decoder = "decoder"

  let add_ast x = x ^^ "_AST"
  let simgen_ast = add_ast simgen
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
  val color_keyword : Color.color option
end

module S_sz (Sz : SIZE_DYN) =
struct
  let mk_ name x = texttt (index "" (Sz.footnote x) ^^ Sz.normal name)
  let mk_r name x = texttt (index (Sz.normal name) (Sz.footnote x))
  let leadsto = \"leadsto\" @ ([], A)
  let lambda_leads = exponent lambda leadsto
  let relbar = \"relbar\" @ ([], M)

  module MK (M : sig val make : Latex.t -> Latex.t end) =
  struct
    let compcert = M.make "compcert"
    let gcc = M.make "gcc"
    let human = M.make "human"
    let asm = M.make "asm"
    let lambda_l = M.make lambda_leads
    let maybe = M.make English.maybe
    let frontc = M.make "frontc"
    let cil = M.make "cil"
    let cparser = M.make "cparser"
    let infty = M.make infty

    let make = M.make
  end

  module C = MK (struct let make = mk_ "C" end)
  module CP = MK (struct let make = mk_ "[C]" end)

  module SL_gen (P_ : sig val sl : Latex.t end) =
  struct
    module M (P_ : sig val sl : Latex.t end) =
    struct
      let add_color s =
        match Sz.color_keyword with
        | None -> s
        | Some c -> Color.textcolor_ c s

      module Make (M : sig val coq : Latex.t val equiv_prefix : Latex.t end) = struct
        let sl x = texttt (add_color "{M.coq}-" ^^ Color.textcolor_ (Color.of_int_255 (let i = 100 in i, i, i)) x ^^ P_.sl)

        module Deep = MK (struct let make x = sl (mk_ "C" x) end)
        module Deep_ocaml = MK (struct let make x = sl (mk_ P.ocaml x) end)
        module Shallow = MK (struct let make = sl end)

      (*let circlearrowleft = \"circlearrowleft\" @ ([], A)*)
        let coq = Shallow.make (*"${circlearrowleft}$"*) M.equiv_prefix
        let coq_deep_ocaml = Deep_ocaml.make ""
      end

      module Coq = Make (struct let coq = P.coq let equiv_prefix = "${equiv}$" end)
      module Ocaml = Make (struct let coq = P.ocaml let equiv_prefix = "{P.coq}${equiv}$" end)
      module Ocaml_deep = Make (struct let coq = P.ocaml let equiv_prefix = "{P.coq}{C.compcert}" end)
      module C = MK (struct let make x = add_color (mk_ "C" x ^^ texttt "-") ^^ texttt P_.sl end)

      let coq = Coq.coq
      let coq_deep_ocaml = Coq.coq_deep_ocaml
    end

    module Arm = M (struct let sl = exponent P_.sl (Sz.footnote "arm") end)
    module Sh = M (struct let sl = exponent P_.sl (Sz.footnote "sh") end)
    module ArmSh = M (struct let sl = P_.sl end)

    include M (P_)
  end

  module SL = SL_gen (struct let sl = P.simlight end)
  module SL2 = SL_gen (struct let sl = P.simlight2 end)
  module Manual = SL_gen (struct let sl = P.manual end)
  module Pseudocode = SL_gen (struct let sl = P.pseudocode end)
  module Decoder = SL_gen (struct let sl = P.decoder end)
  module Simgen = SL_gen (struct let sl = P.simgen_ast end)

  let mk_ml x = texttt ("{P.ocaml}-" ^^ x)
  let simgen = mk_ml P.simgen
  let simgen_ast = mk_ml P.simgen_ast

  module P = SL_gen (struct let sl = "P" end)
end

let interl_ f_vert len val_ = BatList.interleave (f_vert `Vert) (BatList.init len (fun _ -> val_))
let interl x = interl_ (fun x -> x) x

module Comment_sz (SZ_s : SIZE_DYN) (SZ_comment : SIZE_DYN) =
struct
  module S = S_sz (SZ_s)

  let comment m_beg m_end f_tabular f_col f_vert l =
    let f x = [ multirow2 (SZ_comment.footnote (texttt x)) ] in
    let row =
      BatList.take (List.length l)
        (BatList.map f_col [ S.C.human ; S.C.gcc ; S.C.compcert ; S.C.asm ; S.C.lambda_l ]) in
    f_tabular
      SZ_comment.tiny

      (BatList.flatten [ [ `R ] ; interl_ f_vert (List.length row) `C ; [ `L ] ])

      (BatList.map
         (fun l -> Data (BatList.flatten l))
         [ [ f (f_col m_beg) ; row ; f (f_col m_end) ]
         ; BatList.map (BatList.map (fun x -> f_col x))
           [ [ "" ] ; l ; [ "" ] ] ])
end

module Comment =
  Comment_sz
    (struct let normal = scriptsize let footnote = tiny let tiny _ = assert false let color_keyword = None end)
    (struct let normal = normalsize let footnote = footnotesize let tiny = tiny let color_keyword = None end)


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

  module Dot =
  struct
    let l_to_string s =
      BatString.map (function '\n' -> ' ' | x -> x) (Latex.to_string s)

    let verbatim x =
      let () = ignore (List.iter (function `V _ | `C _ -> () | _ -> failwith \"to complete !\") x) in

      let opt, f_after, x =
        match x with
        | `V \"\" :: `C opt :: x ->
          \"--codeonly\", tikzpicture opt, x
        | _ ->
          \"--figonly\", (fun x -> x), x in

      let ic, oc = BatUnix.open_process (sprintf \"dot2tex %s --autosize\" opt) in
      let () =
        begin
          List.iter
            (fun o ->
              BatInnerIO.nwrite oc
                (match o with
                | `V s -> s
                | `C s -> l_to_string s
                | _ -> failwith \"to complete !\"))
            x;
          BatInnerIO.close_out oc;
        end in

      f_after (text (BatInnerIO.read_all ic))
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
    let verbatim = function
        | [ `V x ] -> num_line (fun s -> texttt (footnotesize (LV.verbatim s))) (LV.split_lines (LV.trim ['\n'] x))
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
        | [ `V \"\" ; `C (Comment l) ; `V x ] ->
          let l_head, l_body = Comment.comment "/*" "*/" (fun _ x y -> x, y) (fun x -> Color.textcolor_ Color.comment x) (fun _ -> `Raw (Color.color_ Color.comment ^^ vrule)) l in
          let dim_col = 2 + List.length l in
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


module Version =
struct
  let gcc = P.gcc ^^ " 4.5.2"
  let compcert = P.compcert ^^ " 1.9"
  let compcert_arch = "ia32-linux"
  let ocaml = P.ocaml ^^ " 3.12.1"
  let coq = P.coq ^^ " 8.3pl2"
  let coqsvn = "14161"
  let frontc = "3.1#3"
  let cil = "1.3.6#3"

  module Cpp11 = struct
    let simsoc = "0.7.1"
    let simcert = "1660" (* in svn *)
    let compcert = P.compcert ^^ " 1.8.2"
    module Filename = struct (* it regroups filename used in the report *)
      (*let stat_arm1 = \"stat_arm1_1660\"*)
    end
  end

  module Cpp11_bis = struct
    let simcert = "1789" (* in svn *)
    let compcert = compcert
    module Filename = struct (* it regroups filename used in the report *)
      let stat_arm1 = \"stat_arm1_1789\"
      let stat_arm2 = \"stat_arm2_1789\"
      let stat_armo = \"stat_armocaml_1789\"
    end
  end

end


module S = S_sz (struct let normal = normalsize let footnote = footnotesize let tiny = tiny let color_keyword = None end)
module Sfoot = S_sz (struct let normal = footnotesize let footnote = scriptsize let tiny _ = assert false let color_keyword = None end)
module Ssmall = S_sz (struct let normal = small let footnote = footnotesize let tiny _ = assert false let color_keyword = None end)

module Label =
struct
  let simu_sh4, fast_certi, certi_sim, pretty_print, simgendef, oldarm, ctx_compil, th_init_state, concl, appendix_speed, appendix_eta, appendix_singl =
    label (), label (), label (), label (), label (), label (), label (), label (), label (), label (), label (), label ()

  let ex, newth_ex = Th.newtheorem "Example" ~opt:"subsection"
  let note, newth_note = Th.newtheorem' "Notation"
  let fact, newth_fact = Th.newtheorem "Fact"
  let def, newth_def = Th.newtheorem' "Definition"
  let remark, newth_remark = Th.newtheorem' "Remark"
end


module Performance =
struct

  type simlight =
    | No_simlight
    | Simlight__short
    | Simlight2

  type file =
    | No_thumb of simlight
    | With_thumb (* we assume [With_optim] *)

  type 'a extremum = int (* index of the element in the list *) * 'a

  type 'a bipole = { t_min : 'a extremum ; t_max : 'a extremum }

  type 'a stat = { file : file ; nb_iter : int ; time : 'a list }

  type ('a, 'b) perf = { data : 'b ; t_mima : 'a bipole option }

  let fold_double f =
    let rec aux acc = function
      | x1 :: x2 :: xs -> aux (f acc (x1, x2)) xs
      | [] -> acc
      | _ -> failwith \"fold_double\" in
    aux

  let str_of_float = Printf.sprintf \"%.2f\"
  let str_of_float_percent = Printf.sprintf \"%.1f\"

  module StringMap = BatMap.Make (String)
  module IntMap = BatMap.Make (BatInt)

  let only_one_elt = function [] -> assert false | [_] -> true | _ -> false

  let to_latex map =
    let map = StringMap.fold (fun k { data = (l, i) ; t_mima } -> IntMap.add i { data = (l, k) ; t_mima }) map IntMap.empty in
    List.rev
      (IntMap.fold
         (fun _ { data = (v_perf, name) ; t_mima } l ->
           let row_min_pos, row_max_pos =
             match t_mima with Some { t_min = (row_min_pos, _) ; t_max = (row_max_pos, _) } -> Some row_min_pos, Some row_max_pos | _ -> None, None in
           fst
           (List.fold_left
             (fun (l, row_pos) (name, v) ->
               let opt, thumb =
                 let open English in
                 let is_sl2, thumb_enabled =
                   match v.data.file with
                     | No_thumb No_simlight
                     | No_thumb Simlight__short -> false, false
                     | No_thumb Simlight2 -> true, false
                     | With_thumb -> true, true in
                 let mk_color = function true -> fun f -> f yes | false -> fun f -> Color.textcolor_ (Color.of_int_255 (let i = 100 in i, i, i)) (f no) in
                 mk_color is_sl2 (fun x -> x ^^ ""), mk_color thumb_enabled (fun x -> x ^^ "") in
           Data
             (texttt (Latex.Verbatim.verbatim name)
              ::
              opt
              ::
              thumb
              ::
              latex_of_int v.data.nb_iter
              ::
              let l_tps = v.data.time in
              let (mi_pos, mi), (ma_pos, ma) =
                match v.t_mima with None -> assert false | Some v -> v.t_min, v.t_max in
              let () =
                if mi_pos <> ma_pos then () else assert false (* all values are equal *) in
              BatList.flatten
                [    BatList.mapi (fun pos d ->
                       "{
                         match
                           if pos = mi_pos then Some Color.green_light else if pos = ma_pos then Some Color.red_light else None
                         with
                          | None -> ""
                          | Some color -> Color.cellcolor_ color
                        } {
                           let t = text (str_of_float (d /. float_of_int v.data.nb_iter *. 1000.)) in
                           if not (only_one_elt v_perf) && pos = mi_pos && Some row_pos = row_min_pos then
                             Color.textcolor_ (Color.blue) t
                           else if not (only_one_elt v_perf) && pos = ma_pos && Some row_pos = row_max_pos then
                             Color.textcolor_ (Color.red) t
                           else
                             t
                          }") l_tps
                ; [ Latex.Verbatim.verbatim (Printf.sprintf \"+%s %%\" (str_of_float_percent (100. -. mi *. 100. /. ma)) ^ \"\") ] ])
           ::
           l,
           Pervasives.succ row_pos)
             (Hline :: l, 0)
             (match List.map (fun v -> \"\", v) v_perf with
               | [] -> []
               | (_, x) :: xs -> (name, x) :: xs)))
         map
         [])

  let compute_mima_column l_tps =
    let t_min, t_max, _ =
      List.fold_left
        (fun (o_min, o_max, pos) d ->
          let f_extr f_min o_min =
            match o_min with
              | None -> Some (pos, d)
              | Some (mi_pos, mi) ->
                let mi2 = f_min mi d in
                Some ((if d = mi2 then pos else mi_pos), mi2) in
          f_extr min o_min,
          f_extr max o_max,
          Pervasives.succ pos)
        (None, None, 0)
        l_tps in
    let f = function None -> assert false | Some (i, v) -> i, v in
    Some { t_min = f t_min ; t_max = f t_max }

  let to_latex2 map =
    let map = StringMap.fold (fun k { data = (l, i) ; t_mima } -> IntMap.add i { data = (l, k) ; t_mima }) map IntMap.empty in
    List.rev
      (IntMap.fold
         (fun _ { data = (lll, name) ; _ } l ->
           match lll with
             | [{data = {file = No_thumb No_simlight} as x_ocaml} ; {data = {file = No_thumb Simlight__short} as x_sl} ; {data = {file = No_thumb Simlight2} as x_sl2}]
             | [{data = {file = No_thumb No_simlight} as x_ocaml} ; {data = {file = No_thumb Simlight__short} as x_sl} ; {data = {file = No_thumb Simlight2} as x_sl2} ; _ ] ->

           let row_min_pos, row_max_pos = None, None in
           let l = Hline :: l in
           fst
           (
             (
               let row_pos = 0 in
               let opt, thumb =
                 let open English in
                 let is_sl2, thumb_enabled = false, false in
                 let mk_color = function true -> fun f -> f yes | false -> fun f -> Color.textcolor_ (Color.of_int_255 (let i = 100 in i, i, i)) (f no) in
                 mk_color is_sl2 (fun x -> x ^^ ""), mk_color thumb_enabled (fun x -> x ^^ "") in
           Data
             (texttt (Latex.Verbatim.verbatim name)
              ::
              let l_tps = List.map (fun (v, n) -> List.nth v.time n, v.nb_iter) [ x_ocaml, 0 ; x_sl, 0 ; x_sl2, 0 ; x_sl, 4 ; x_sl2, 4 ] in
              let (mi_pos, mi), (ma_pos, ma) =
                match compute_mima_column (BatList.map (fun (f, nb) -> f /. float_of_int nb) l_tps) with None -> assert false | Some v -> v.t_min, v.t_max in
              let () =
                if mi_pos <> ma_pos then () else assert false (* all values are equal *) in
              BatList.flatten
                [    BatList.mapi (fun pos (d, nb_iter) ->
                       "{
                         match
                           if pos = mi_pos then Some Color.green_light else if pos = ma_pos then Some Color.red_light else None
                         with
                          | None -> ""
                          | Some color -> Color.cellcolor_ color
                        } {
                           let t = text (str_of_float (d /. float_of_int nb_iter *. 1000.)) in
                           (*if not (only_one_elt v_perf) && pos = mi_pos && Some row_pos = row_min_pos then
                             Color.textcolor_ (Color.blue) t
                           else if not (only_one_elt v_perf) && pos = ma_pos && Some row_pos = row_max_pos then
                             Color.textcolor_ (Color.red) t
                           else*)
                             t
                          }") l_tps
                ; [ Latex.Verbatim.verbatim (Printf.sprintf \"+%s %%\" (str_of_float_percent (100. -. mi *. 100. /. ma)) ^ \"\") ] ])
           ::
           l,
           Pervasives.succ row_pos)
             )
             | _ ->
               if List.exists (function {data = {file = No_thumb No_simlight}} -> true  | _ -> false ) lll then
                 Printf.kprintf failwith \"%s %d\" name (List.length lll)
               else
                 l
         )
         map
         [])

  let performance_of_file f_mima (f_simlight, file) map =
      fold_double
        (fun (map, dim_map) (name, s) ->
          let name = BatString.trim name in
          let nb_iter :: l_tps = BatList.filter_map (function \"\" -> None | s -> Some s) (BatString.nsplit s \" \") in
          let l_tps =
            BatList.map float_of_string l_tps in
          let t_mima = f_mima l_tps in
          let name, file =
            let und = \"_\" in
            let l, file =
              match List.rev (BatString.nsplit name und) with
                | \"a\" :: l -> l, No_thumb f_simlight
                | \"t\" :: l -> l, With_thumb
                | _ -> Printf.kprintf failwith \"file %S does not end with a recognized extension\" name in
            BatString.join und (List.rev l), file in

          let dim_map, index =
            match StringMap.Exceptionless.find name map with
              | Some (_, index) -> dim_map, index
              | None -> let d = Pervasives.succ dim_map in d, d in

          StringMap.modify_def
            ([], index)
            name
            (fun (l, index) -> { data = { file ; nb_iter = int_of_string nb_iter ; time = l_tps } ; t_mima } :: l, index)
            map,
          dim_map)
        map
        (BatList.filter
           (function \"\" -> false | _ -> true)
           (BatString.nsplit
              (BatFile.with_file_in file BatIO.read_all) \"\n\"))

  let compute_mima_row f_extr =
    StringMap.map
      (fun (l, i) ->

        let l =
          List.fast_sort
            (fun v1 v2 ->
              let f v = match v.data.file with
                | No_thumb No_simlight -> -1
                | No_thumb Simlight__short -> 0
                | No_thumb Simlight2 -> 1
                | With_thumb -> 2 in
              compare (f v1) (f v2))
            l in

        { data = l, i
        ; t_mima = f_extr l })

  let stat_file1 =
    [ Simlight__short, Version.Cpp11_bis.Filename.stat_arm1
    ; Simlight2, Version.Cpp11_bis.Filename.stat_arm2 ]

  let stat_file2 =
    (No_simlight, Version.Cpp11_bis.Filename.stat_armo)

  let map_perf f_compute_mima_column =
    List.fold_left
      (fun map sl_file -> performance_of_file f_compute_mima_column sl_file map)
      (StringMap.empty, 0)
      stat_file1

  let draw_performance1 f_tabular =
    f_tabular (BatList.flatten [ `L :: `C :: `C :: `Vert :: interl 7 `R ])
      (BatList.flatten
         [ (let module S = Ssmall in
            let f n = [ "{texttt "gcc -m32 -O{latex_of_int n}"}" ] in
            title
             (fun x -> x)
             [ ]
             None
             [ [ "ARMv6 Executable and Linkable Format (ELF)" ]
             ; [ "Do we execute with the optimized version of the simulator ?" ]
             ; [ "Was the ELF code specially generated for the Thumb ?" ]
             ; [ "Number of iterations needed to have a significant duration" ; "in seconds for a human (currently, beyond 4 seconds)" ]
             ; [ "Time in milliseconds of 1 iteration" ; "(i.e. the total time divided by the number of iterations)" ; "with {S.SL.C.asm}, compiled by" ; "{P.compcert} {texttt Version.compcert_arch}" ]
             ; "with {S.SL.C.gcc}, compiled by" :: f 0
             ; f 1
             ; f 2
             ; f 3
             ; [ "Relative" ; "gain" ; "between" ; "max" ; "and" ; "min" ] ])
         ; to_latex
           (compute_mima_row
              (fun l ->
                let fold get_min f_min =
                  let get_min v = snd (get_min v) /. float_of_int v.data.nb_iter in
                  match
                    List.fold_left
                      (fun (o, pos) v ->
                        let v_min = get_min v in
                        (match o with
                          | None -> Some (pos, v_min)
                          | Some (pos1, min1) ->
                            let min2 = f_min min1 v_min in
                            Some ((if min2 = v_min then pos else pos1), min2)),
                        Pervasives.succ pos)
                      (None, 0)
                      l
                  with
                    | None, _ -> assert false
                    | Some p, _ -> p in

                Some { t_min = fold (fun v -> match v.t_mima with None -> assert false | Some v -> v.t_min) min
                     ; t_max = fold (fun v -> match v.t_mima with None -> assert false | Some v -> v.t_max) max })
              (fst (map_perf compute_mima_column))) ])

  let draw_performance2 f_tabular =
    f_tabular (BatList.flatten [ `L :: `Vert :: interl 6 `R ])
      (BatList.flatten
         [ (let module S = Ssmall in
            let f optim _ =
              let l_of_bool b = if b then English.yes else English.no in
              "({l_of_bool optim} opt)" in
            title
             (fun x -> x)
             [ ]
             None
             [ [ "ARMv6 Executable and Linkable Format ({English.no} Thumb generation)" ]
             ; [ "Time in milliseconds of 1 iteration"
               ; "with {S.SL.Ocaml.coq} {f false false}," ; "compiled by {texttt "ocamlopt"}" ]
             ; [ "with {S.SL.C.asm} {f false false}," ; "compiled by {P.compcert} {texttt Version.compcert_arch}" ]
             ; [ "with {S.SL.C.asm} {f true false}," ; "compiled by {P.compcert} {texttt Version.compcert_arch}" ]
             ; [ "with {S.SL.C.gcc} {f false false}," ; "compiled by {texttt "gcc -m32 -O3"}" ]
             ; [ "with {S.SL.C.gcc} {f true false}," ; "compiled by {texttt "gcc -m32 -O3"}" ]
             ; [ "Relative gain" ; "between max and min" ] ])
         ; to_latex2
           ((*StringMap.map
              (fun v ->
                { v with data =
                    match v.data with
                      | { data = [ sl_ocaml ; sl1 ; sl2 ] ; t_mima = None } as v, i -> { v with t_mima = Some () }, i
                      | _ -> assert false }) *)
              (compute_mima_row (fun _ -> None) (fst (performance_of_file (fun _ -> None) stat_file2 (map_perf (fun _ -> None)))))) ])
end

let th_same_source l =
  let l, last =
    let last :: l = List.rev l in
    List.rev l, last in
  Th.env Label.fact "
However their possible different behavior at runtime, {concat (BatList.map (fun x -> x ^^ ", ") l)}and {last} come from an initial same source. This source belongs to :
{ let module Comment = Comment_sz
        (struct let normal = normalsize let footnote = footnotesize let tiny = tiny let color_keyword = None end)
        (struct let normal = normalsize let footnote = large3 let tiny = small let color_keyword = None end) in
  let open English in Comment.comment "<<{>>" "<<}>>" (fun f_tiny x y -> newline ^^ f_tiny (tabular x y)) (fun x -> x) (fun x -> x) (BatList.init (List.length l + 2) (fun _ -> yes)) }
"
