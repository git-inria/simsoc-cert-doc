open Melt_lib open L
open Melt_highlight
open Simsoc_cert

##verbatim '?' = Code.Raw_.verbatim
##verbatim '!' = Code.Raw.verbatim
##verbatim '#' = Code.Coq.verbatim
##verbatim 'Z' = Code.Coq.verbatim
##verbatim '~' = Code.Ml.verbatim
##verbatim '@' = Code.Humc_.verbatim
##verbatim 'O' = Code.Dot.verbatim
##verbatim 'X' = Code.Raw__.verbatim

open Printf

module Argument = struct
  let dir_init = Sys.argv.(1)

  let dir_img = sprintf \"%s/doc/img/%d.png\" dir_init
  let file_arm6 = sprintf \"%s/../../simsoc-cert-cc1.9/arm6/arm6.pdf\" dir_init
  let page_middle = sprintf \"%s/doc/img/page_middle.png\" dir_init

  let img1 = dir_img 1
  let img2 = dir_img 2
  let img3 = dir_img 3
  let img4 = dir_img 4
end

let red = Color.textcolor_ (Color.of_int_255 (0x9F, 0x00, 0x00))

let () =
  main
    ~packages:[]
    ~author:[ footnotesize (mail \"tuong@users.gforge.inria.fr\") ]

    (Beamer (B.Abr
[ B.Center ("", "")

(* ********************************************************* *)
; B.Abr (BatList.map
           (fun (page, x, y, trim_top) ->
             B.Center
               ("Example: ARMv6 manual, AND instruction, page " ^^ latex_of_int page,
                includegraphics ~x ~y ~trim:(`Mm 0., `Mm 0., `Mm 0., `Mm trim_top) ~page ~scale:0.9 Argument.file_arm6))
           [ 158, 4.9, -5.5, 25.
           ; 159, 4.6, -6., 20. ])

(* ********************************************************* *)
; B.Center ("{P.simcert}, code generation from the manual",
            let deepskyblue = Color.of_int_255 (0x00, 0xBF, 0xFF) in
            let module S = S_sz (struct let normal = normalsize let footnote = footnotesize let tiny = tiny let color_keyword = Some deepskyblue end) in
            let dodgerblue = Color.color_name_ (Color.of_int_255 (0x1E, 0x90, 0xFF)) in
            let floralwhite = "deepskyblue" in
"<OO{text \"scale=0.6, every node/.style={transform shape}, overlay, shift={(-1.3, -8.3)}\"}O
digraph G {
  margin=0
  compound=true
  node [color=grey, shape=note, color=darksalmon, fontname="Gill Sans", height=0.1]
  ranksep=0.001
  nodesep=0.001

  arm_pdf [texlbl="ARMv6.pdf"]
  arm_txt [texlbl="ARMv6.txt"]

  patch1 [shape=none, texlbl="patch \& extract"]
  patch2 [shape=none, texlbl="patch \& extract"]
  patch3 [shape=none, texlbl="patch \& extract"]

  subgraph cluster_ocaml {
    style="dashed, rounded"
    color=O{floralwhite}O
    ast_deco [texlbl="IS encoding"]
    ast_asm [texlbl="ASM syntax"]
    ast_pseudo [texlbl="pseudo-code"]

    intern_merge [shape=none, texlbl="merge \& preprocess"]
    intern_ocaml [texlbl="internal representation of the AST in O{Color.textcolor_ deepskyblue P.ocaml}O"]

    out_coq_ [shape=none, texlbl="O{multiline_ \"monadic higher order\nfactoring\"}O"]
    out_simlight_ [shape=none, texlbl="O{multiline_ \"normalization, flattening\nand specialization\"}O"]
  }

  out_coq [style=filled, fillcolor=papayawhip, texlbl="shallow embedding to O{Color.textcolor_ deepskyblue "Coq"}O"]
  out_coq_to [shape=none, texlbl="copy"]
  out_simlight [style=filled, fillcolor=papayawhip, texlbl="fast ISS (O{Color.textcolor_ deepskyblue S.C.gcc}O/C++)"]
  out_simlight_to [shape=none, texlbl="copy"]

  subgraph cluster_simsoc {
    style="dashed, rounded"
    color=O{floralwhite}O
    cluster_simsoc_title [shape=none, texlbl="O{multiline_ \"SimSoC\n(C++/SystemC)\"}O"]
    subgraph cluster_simsoc_arm {
      style=dotted
      cluster_simsoc_arm_title [shape=none, texlbl="O{multiline ["ARMv6" ; S.SL.C.gcc ]}O"]
      simsoc_mmu [texlbl="MMU"]
      simsoc_iss [style=filled, fillcolor=papayawhip, texlbl="fast ISS (O{Color.textcolor_ deepskyblue S.C.gcc}O/C++)"]
    }
    simsoc_peri [texlbl="O{multiline_ \"memory\nand peripherals\"}O"]
  }

  subgraph cluster_simcoq {
    style="dashed, rounded"
    color=O{floralwhite}O
    cluster_simcoq_title [shape=none, texlbl="O{multiline [ "ARMv6" ; S.SL.coq ]}O"]
    simcoq_iss [style=filled, fillcolor=papayawhip, texlbl="O{multiline [ "shallow" ; "embedding" ; "to {Color.textcolor_ deepskyblue "Coq"}" ]}O"]
  }

  /* */
  cluster_simsoc_title -> cluster_simsoc_arm_title -> simsoc_iss -> simsoc_mmu -> simsoc_peri -> out_simlight_to [style=invis]
  cluster_simcoq_title -> simcoq_iss [style=invis]

  /* */
  edge [color=O{dodgerblue}O]
  arm_pdf -> arm_txt [label="pdftotext", constraint=false]
  arm_txt -> patch1 -> ast_deco
  arm_txt -> patch2 -> ast_asm
  arm_txt -> patch3 -> ast_pseudo

  ast_deco -> intern_merge
  ast_asm -> intern_merge
  ast_pseudo -> intern_merge

  intern_merge -> intern_ocaml

  intern_ocaml -> out_coq_ -> out_coq
  intern_ocaml -> out_simlight_ -> out_simlight

  simcoq_iss -> out_coq_to [dir=back]
  out_coq_to -> out_coq [dir=back]

  simsoc_iss -> out_simlight_to [dir=back]
  out_simlight_to -> out_simlight [dir=back]
}
O>")

(* ********************************************************* *)
; B.Center (let page = 234 in "Example: SH4 manual, AND instruction, page " ^^ latex_of_int page, includegraphics ~x:5.5 ~y:(-.5.0) ~scale:0.7 \"sh4_and.pdf\")

(* ********************************************************* *)
; B.Center ("Example: SH4 manual, AND instruction, page 234 middle", includegraphics ~x:5.25 ~y:(-1.) ~scale:0.4 Argument.page_middle)

(* ********************************************************* *)
; B.Abr (BatList.map
           (fun (x, trim_top, img) ->
             B.Center ("Patching the SH4 manual, example ", includegraphics ~x ~y:(-1.) ~trim:(`Mm 0., `Mm 0., `Mm 0., `Mm trim_top) ~scale:0.6 img))
           [ 5.5, 55., Argument.img1
           ; 5., 52., Argument.img3 ])

(* ********************************************************* *)
; B.Center ("Patch generation in OCaml",
"<~
[ [ Replace_all ("&&", "&"), p [ 1065 ]
    (* not Coq well typed otherwise *)
  ; Replace_all ("(long i)", "i"), [ R (1077, 2) ] ]
; [ comment_first ", int *FPUL", p [ 4003 ; 4008 ; 6913 ; 6918 ]
  ; Replace_first (S "*FPUL", "FPUL"), p [ 4005 ; 4010 ; 6915 ; 6921 ; 2113 ; 2116 ] ]
; [ Replace_all ("TLB[MMUCR. URC] ", "TLB_MMUCR_URC"), [ R (4162, 13) ] ]
; [ Replace_first (S "MACL", "MACL_"), p [ 4222 ]
  ; Replace_first (S "STS", "STS_"), p [ 6924 ] ]
(* type error *)
; [ Replace_first (S "MACH&0x00008000",
                   "bool_of_word(MACH&0x00008000)"), p [ 4290 ] ]
(* simplifying *)
; [ Replace_first (All, "if_is_write_back_memory_and_look_up_in_operand_cache_eq_miss_then_allocate_operand_cache_block(R[n]);"), [ R (5133, 3) ]
  ; Replace_first (Re "(is_dirty_block(R\\[n\\])) +write_back(R\\[n\\]);?"
                  , "_is_dirty_block_then_write_back(R[n]);"), p [ 5502 ; 5543 ] ] ]
~>")

(* ********************************************************* *)
; B.Center ("Patching the SH4 manual",
            "Patching data = 8K (without blanks)" ^^ vspace (`Em 1.)
            ^^
            let perc n =
              Color.textcolor_ (Color.of_int_255 (0xCF, 0x5C, 0x16)) ((latex_of_int n) ^^ "%") in
            tabular [`L;`Vert;`R;`Vert;`R;`Vert;`R;`Vert;`R]
              [ Data ["SH4 manual"; "words" ; "common" ; "" ; "changed" ]
              ; Hline
              ; Data [""; "" ; "" ; "deleted" ; "" ]
              ; Data ["original.txt" ; "86980" ; "85070 {perc 98}" ; "499 {perc 1}" ; "1411 {perc 2}"]
              ; Hline
              ; Data ["generated"; "" ; "" ; "inserted" ; "" ]
              ; Data ["patched.txt" ; "87372" ; "85070 {perc 97}" ; "872 {perc 1}" ; "1430 {perc 2}"] ])

(* ********************************************************* *)
; B.Abr (BatList.map
           (fun (fct_edge, darksalmon, draw_red, attr_error) ->
             B.Bottom
               ("CompCert, semantic preservation proved in {P.coq}",
                minipage (`Cm 3.5)
                  (Th.env Label.note (itemize [ "``{red S.C.compcert}'': first AST defined in Coq"
                                              ; "``{red S.C.asm}'': last AST defined in Coq"
                                              ]))
                ^^
                let shift_x, shift_y = -3.5, -0.2 in
                let edge_to_fct dir = concat [ "[" ; (match dir with `normal -> "" | `back -> "dir=back,") ; "color={darksalmon}]" ] in
"<OO{text (sprintf \"every node/.style={transform shape}, overlay, shift={(%f, %f)}\" shift_x shift_y)}O
digraph G {
  margin=0
  compound=true
  node [color=grey, shape=box, style=rounded, color=mediumseagreen, fontname="Gill Sans", height=0.1]
  ranksep=0.01
  nodesep=0.1

  /* nodes */
  compcert_c [texlbl="O{S.C.compcert}O"]
  clight [texlbl="Clight"]
  c_minor [texlbl="CO{symbolc '#'}Ominor"]
  cminor [texlbl="Cminor"]
  cminorsel [texlbl="CminorSel"]
  rtl [texlbl="RTL"]
  ltl [texlbl="LTL"]
  ltlin [texlbl="LTLin"]
  linear [texlbl="Linear"]
  mach [texlbl="Mach"]
  asm [texlbl="O{S.C.asm}O"]

  error O{attr_error}O

  /* nodes: "fct" version */
  compcert_c_fct [shape=point, style=invis]
  clight_fct [shape=point, style=invis]
  c_minor_fct [shape=point, style=invis]
  cminor_fct [shape=point, style=invis]
  cminorsel_fct [shape=point, style=invis]
  rtl_fct [shape=point, style=invis]
  ltl_fct [shape=point, style=invis]
  ltlin_fct [shape=point, style=invis]
  linear_fct [shape=point, style=invis]
  mach_fct [shape=point, style=invis]

  /* */
  compcert_c -> rtl [style=invis]
  clight -> cminorsel [style=invis]
  cminorsel -> ltlin [style=invis]
  cminor -> linear [style=invis]
  ltlin -> asm [style=invis]

  compcert_c_fct -> cminorsel_fct [style=invis]
  clight_fct -> cminor_fct [style=invis]
  cminorsel_fct -> ltl_fct [style=invis]
  cminor_fct -> ltlin_fct [style=invis]
  ltlin_fct -> mach_fct [style=invis]

  /* */
  { rank = same ;
    compcert_c -> compcert_c_fct O{edge_to_fct `normal}O
    compcert_c_fct -> clight [color=O{draw_red}O]
    clight -> clight_fct O{edge_to_fct `normal}O
    clight_fct -> c_minor [color=O{draw_red}O] }
  c_minor -> c_minor_fct O{edge_to_fct `normal}O
  c_minor_fct -> cminor [color=O{draw_red}O]
  { rank = same ;
    rtl -> cminorsel_fct [dir=back, color=O{draw_red}O]
    cminorsel_fct -> cminorsel O{edge_to_fct `back}O
    cminorsel -> cminor_fct [dir=back, color=O{draw_red}O]
    cminor_fct -> cminor O{edge_to_fct `back}O }
  rtl -> rtl_fct O{edge_to_fct `normal}O
  rtl_fct -> ltl [color=O{draw_red}O]
  { rank = same ;
    ltl -> ltl_fct O{edge_to_fct `normal}O
    ltl_fct -> ltlin [color=O{draw_red}O]
    ltlin -> ltlin_fct O{edge_to_fct `normal}O
    ltlin_fct -> linear [color=O{draw_red}O] }
  linear -> linear_fct O{edge_to_fct `normal}O
  linear_fct -> mach [color=O{draw_red}O]
  { rank = same ;
    asm -> mach_fct [dir=back, color=O{draw_red}O]
    mach_fct -> mach O{edge_to_fct `back}O }

  /* */
  compcert_c_fct -> error O{fct_edge}O
  clight_fct -> error O{fct_edge}O
  c_minor_fct -> error O{fct_edge}O
  cminor_fct -> error O{fct_edge}O
  cminorsel_fct -> error O{fct_edge}O
  rtl_fct -> error O{fct_edge}O
  ltl_fct -> error O{fct_edge}O
  ltlin_fct -> error O{fct_edge}O
  linear_fct -> error O{fct_edge}O
  mach_fct -> error O{fct_edge}O

}
O>")
           )
           [ ( let darksalmon = Color.color_name_ (Color.of_int_255 (0x3C, 0xB3, 0x71)) in
               "[style=invis]"
             , darksalmon
             , darksalmon
             , "[texlbl=\"{phantom "monadic error"}\", color=white]" )

           ; ( "[color={Color.color_name_ (Color.of_int_255 (0xF5, 0xA7, 0x5F))}]"
             , Color.color_name_ (Color.of_int_255 (0x3C, 0xB3, 0x71))
             , Color.color_name_ (let i = 200 in Color.of_int_255 (i, i, i))
             , "[texlbl=\"monadic error\"]" ) ])

(* ********************************************************* *)
; B.Top ("CompCert, the generation{newline}from {P.coq} to {P.ocaml}",
         minipage (`Cm 4.2)
           (Th.env Label.note (concat [ "``{red S.C.human}'': an arbitrary sequence of character"
                                      ; newline
                                      ; "``{red S.C.compcert}'': programs compiled successfully with {Version.compcert} <!-dc!>"
                                      ; newline
                                      ; "``{red S.C.asm}'': programs compiled successfully with {Version.compcert} <!-dasm!>" ]))
         ^^
         let darksalmon_ = Color.of_int_255 (0xF5, 0xA7, 0x5F) in
         let darksalmon = Color.color_name_ darksalmon_ in
         let mediumseagreen_ = Color.of_int_255 (0x3C, 0xB3, 0x71) in
         let mediumseagreen = Color.color_name_ mediumseagreen_ in
         let deepskyblue = Color.of_int_255 (0x00, 0xBF, 0xFF) in
         let floralwhite = "deepskyblue" in
         let ocaml = texttt (Color.textcolor_ deepskyblue P.ocaml) in
         let coq = texttt (Color.textcolor_ deepskyblue P.coq) in
         let gcc = texttt (Color.textcolor_ deepskyblue S.C.gcc) in
         let col_fct = Color.textcolor_ darksalmon_ in
         let black s = small (emph (Color.textcolor_ (let i = 0x86 in Color.of_int_255 (i, i, i)) s)) in
         let green = Color.textcolor_ mediumseagreen_ in
"<OO{text \"scale=0.9, every node/.style={transform shape}, overlay, shift={(-2.6, -6.3)}\"}O
digraph G {
  margin=0
  compound=true
  node [color=grey, shape=box, style=rounded, color=mediumseagreen, fontname="Gill Sans", height=0.1]
  ranksep=0.01
  nodesep=0.1

  subgraph cluster_coq {
    style="dashed, rounded"
    color=O{floralwhite}O

    compcert_c [texlbl="O{S.C.compcert}O"]
    asm [texlbl="O{S.C.asm}O"]

    compcert_c_fct [texlbl="O{multiline [ col_fct (ocaml ^^ "-compiling") ; black ("(generated from " ^^ coq ^^ ")")]}O", color=darksalmon]

    compcert_c -> compcert_c_fct [color=O{mediumseagreen}O]
  }

  subgraph cluster_other1 {
    style="dashed, rounded"
    color=O{floralwhite}O

    human_c [texlbl="O{S.C.human}O"]
    human_c_fct [texlbl="O{multiline [ col_fct (gcc ^^ "-preprocessing {green "{longrightarrow}"} " ^^ ocaml ^^ "-parsing") ; black ("(not yet generated from " ^^ coq ^^ ")") ]}O", color=darksalmon]
    human_c -> human_c_fct [color=O{mediumseagreen}O]

  }

  subgraph cluster_other2 {
    style="dashed, rounded"
    color=O{floralwhite}O

    asm_fct [texlbl="O{multiline [ col_fct (ocaml ^^ "-printing {green "{longrightarrow}"} " ^^ gcc ^^ "-linking") ; black ("(not yet generated from " ^^ coq ^^ ")") ] }O", color=darksalmon]
    exec [texlbl="executable"]
    asm_fct -> exec [color=O{mediumseagreen}O]
  }

  human_c_fct -> compcert_c [color=O{mediumseagreen}O]
  compcert_c_fct -> asm [color=O{mediumseagreen}O]
  asm -> asm_fct [color=O{mediumseagreen}O]

  error [texlbl="error"]
  compcert_c_fct -> error [color=O{darksalmon}O]
  human_c_fct -> error [color=O{darksalmon}O]
  asm_fct -> error [color=O{darksalmon}O]
}
O>")

(* ********************************************************* *)
; B.Center ("Patching the SH4 manual, example ", includegraphics ~x:5.5 ~y:(-1.) ~trim:(`Mm 0., `Mm 0., `Mm 0., `Mm 52.) ~scale:0.6 Argument.img2)

(* ********************************************************* *)
; B.Center ("ss",
"<@@{let open English in H_comment [ yes ; yes ; no ] }@
struct S { unsigned long i:1 }
main () {}
@>")

(* ********************************************************* *)
; B.Center ("Patching the SH4 manual, example ", includegraphics ~x:5.5 ~y:(-1.) ~trim:(`Mm 0., `Mm 0., `Mm 0., `Mm 55.) ~scale:0.6 Argument.img4)

(* ********************************************************* *)
; B.Center ("z",
            th_same_source [ S.Manual.Sh.C.gcc ; S.Manual.Sh.C.compcert ])

(* ********************************************************* *)
; B.Center ("zz",
            Th.env Label.fact "
Besides some minor modifications, the existing framework generating the {S.Manual.Arm.coq} can completely be used in the same way to produce the {S.Manual.Sh.coq}.")

(* ********************************************************* *)
; B.Center ("{S.Decoder.ArmSh.C.human}",
"Decode a given word (16, 32 bits...) to instruction to execute.
<Z
Definition Z{PPP}Z := match Z{PPP}Z with
(*9.1.0 - ADD*)
| word16 0 0 1 1 _ _ _ _ _ _ _ _ 1 1 0 0 =>
  DecInst _ (ADD (regnum_from_bit n4 w) (regnum_from_bit n8 w))
(*9.1.1 - ADDI*)
| word16 0 1 1 1 _ _ _ _ _ _ _ _ _ _ _ _ =>
  DecInst _ (ADDI w[n7#n0] (regnum_from_bit n8 w))
(*9.2.0 - ADDC*)
| word16 0 0 1 1 _ _ _ _ _ _ _ _ 1 1 1 0 =>
  DecInst _ (ADDC (regnum_from_bit n4 w) (regnum_from_bit n8 w))
Z{PPP}Z
(*9.103.0 - XTRCT*)
| word16 0 0 1 0 _ _ _ _ _ _ _ _ 1 1 0 1 =>
  DecInst _ (XTRCT (regnum_from_bit n4 w) (regnum_from_bit n8 w))
| _ => DecUndefined_with_num inst 0
end.
Z>"
)

(* ********************************************************* *)
; B.Center ("{P.simcert}, towards the correctness proof",
            let deepskyblue = Color.of_int_255 (0x00, 0xBF, 0xFF) in
            let module S = S_sz (struct let normal = normalsize let footnote = footnotesize let tiny = tiny let color_keyword = Some deepskyblue end) in
            let module SL_p = S.SL_gen (struct let sl = "PROGRAM" end) in
            let dodgerblue = Color.color_name_ (Color.of_int_255 (0x1E, 0x90, 0xFF)) in
            let firebrick = Color.color_name_ (Color.of_int_255 (0xB2, 0x22, 0x22)) in
            let floralwhite = "deepskyblue" in
"<OO{text \"scale=0.8, every node/.style={transform shape}, overlay, shift={(-1.55, -6.6)}\"}O
digraph G {
  margin=0
  compound=true
  node [color=grey, shape=box, fontname="Gill Sans"]
  ranksep=0.01
  nodesep=0.1

  pdf_sh [shape=note, color=darksalmon, texlbl="O{multiline [S.Manual.Sh.C.human ; "(pdf)"]}O"]
  pdf_txt_sh [shape=note, color=darksalmon, style=filled, fillcolor=papayawhip, texlbl="O{multiline [S.Manual.Sh.C.human ; "(txt)"]}O"]

  pseudocode [shape=ellipse, style=dashed, color=dodgerblue, texlbl="O{ multiline [ "pseudo-code and" ; "decoder generator" ] }O"]

  subgraph cluster_simsoc {
    style="dashed,rounded"
    color=O{floralwhite}O

    subgraph cluster_simlight {
      style=bold
      color=darksalmon
      iss [shape=note, color=darksalmon, style=filled, fillcolor=papayawhip, texlbl="O{S.Manual.Sh.C.compcert}O"]
      simlight_dots [shape=note, color=darksalmon, texlbl="O{ multiline [ S.SL.C.compcert ; "libraries" ] }O"]
    }
  }

  subgraph cluster_coq {
    style="dashed,rounded"
    color=O{floralwhite}O

    mid [shape=ellipse, style=dashed, color=dodgerblue, texlbl="O{ multiline [ "deep embedded" ; "pretty-printer" ; "for {S.C.compcert}" ] }O"]

    subgraph cluster_compcert_simlight {
      style=bold
      color=darksalmon
      bgcolor=papayawhip
      coq_src2 [shape=note, color=darksalmon, style=filled, fillcolor=papayawhip, texlbl="O{S.Manual.Sh.Coq.Deep.compcert}O"]
      coq_src_dot [shape=note, color=darksalmon, texlbl="O{ multiline [ S.SL.Coq.Deep.compcert ; "libraries" ] }O"]
    }

    subgraph cluster_simulateur {
      style=bold
      color=darksalmon
      coq_src1 [shape=note, color=darksalmon, style=filled, fillcolor=papayawhip, texlbl="O{S.Manual.Sh.coq}O"]
      coq_src_simul_dot [shape=note, color=darksalmon, texlbl="O{ multiline [ S.SL.coq ; "libraries" ] }O"]
    }

    coq_proof [shape=note, color=firebrick, texlbl="O{ multiline [ "Correctness proof :" ; S.SL.coq ; "{leftrightarrow_}" ; "{S.SL.Coq.Deep.compcert} ?" ] }O"]
  }

  /* */
  iss -> simlight_dots [style=invis]
  coq_src2 -> coq_src_dot [style=invis]
  coq_src1 -> coq_src_simul_dot [style=invis]
  pdf_sh -> coq_src1 [style=invis]

  /* */
  edge [color=O{dodgerblue}O]
  pdf_sh -> pdf_txt_sh [label="pdftotext", constraint=false]
  pdf_txt_sh -> pseudocode [constraint=false]
  pseudocode -> iss
  pseudocode -> coq_src1
  simlight_dots -> mid [ltail=cluster_simlight]
  coq_src_dot -> mid [ltail=cluster_compcert_simlight, dir=back]
  coq_src_dot -> coq_proof [color=O{firebrick}O, ltail=cluster_compcert_simlight]
  coq_src_simul_dot -> coq_proof [color=O{firebrick}O, ltail=cluster_simulateur]
}
O>")

(* ********************************************************* *)
; B.Center ("ast",
"<#
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
#>")

(* ********************************************************* *)
; B.Center ("",
"<#
Check _floatsize : floatsize -> s.
Check _type : type -> s.
Check _typelist : typelist -> s.
Check _ident : ident -> s.
Check _program : forall A, (A -> s) -> program A -> s.
Check _ast : ast -> s.
#>
")

(* ********************************************************* *)
; B.Center ("",
"<#
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
#>")

(* ********************************************************* *)
; B.Center ("",
"<#
  Notation "A ** n" := (A ^^ n --> A) (at level 29) : type_scope.

Check _INDUCTIVE : string -> forall n, s ** n.
  Notation "| x" := (_INDUCTIVE x _) (at level 9).

Check _RECORD : forall n, vector string n -> s ** n.
  Notation "{{ a ; .. ; b }}" :=
    (_RECORD _ (Vcons _ a _ .. (Vcons _ b _ (Vnil _)) ..)).
#>
")

(* ********************************************************* *)
; B.Center ("",
"<#
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
")

(* ********************************************************* *)
; B.Center ("zz",
            let module SL_p = S.SL_gen (struct let sl = "PROGRAM" end) in
            Th.env Label.fact "
The pretty-printer defined can be used to parse an arbitrary {SL_p.C.compcert} to a Coq representation. For the rest, this associated deep embedded Coq program will be named as : {SL_p.Coq.Deep.compcert}.
")

(* ********************************************************* *)
; B.Center ("zz",
            let module SL_p = S.SL_gen (struct let sl = "PROGRAM" end) in
            Th.env Label.fact "Because the internal processing {texttt "f"} going from a {S.C.compcert} representation to a {S.C.asm} representation in {P.compcert} is written in Coq, we can name {SL_p.Coq.Deep.asm} the mapping of {texttt "f"} to the {SL_p.Coq.Deep.compcert} associated to an initial {S.C.asm}.

Therefore, we now have a way to parse an arbitrary {S.C.asm} file to Coq.
")

(* ********************************************************* *)
; B.Center ("",
"<@@{let open English in H_comment [ yes ; yes ; no ; no ]}@
#include <inttypes.h>

void main() {
  int64_t x = 0;
}
@>")

(* ********************************************************* *)
; B.Center ("",
            let blue = Color.textcolor_ Color.blue in
"<@@{let open English in H_comment (BatList.init 4 (fun _ -> yes))}@
#include <stdio.h>

void main() {
  int i = 32;
  printf("line 1 %lx\n", 1lu <<  i);
  printf("line 2 %lx\n", 1lu << 32);
}
@>
{
texttt (tabular (interl 4 `L)
    [ Data [ "" ; "gcc -m64" ; "gcc -m32 -O0{textrm ","}" ; "gcc -m32 -O1" ]
    ; Data [ "" ; "" ; "{textrm P.compcert}" ; "gcc -m32 -O2" ]
    ; Data [ "" ; "" ; "" ; "gcc -m32 -O3" ]
    ; Hline
    ; Data [ "line 1" ; blue "100000000" ; blue "1" ; blue "0" ]
    ; Data [ "line 2" ; blue "100000000" ; blue "0" ; blue "0" ] ]) }

(in OCaml: {blue "<!Int32.shift_left 1_l 32!>"} {longrightarrow} {blue "<!1_l!>"})")

(* ********************************************************* *)
; B.Center ("", th_same_source [ S.SL.C.gcc ; S.SL.C.compcert ; S.SL.C.asm ])

(* ********************************************************* *)
; B.Center ("zz",
            let s = "S" in
            let module SL_p = S.SL_gen (struct let sl = s end) in
            Th.env Label.def "
We define :
{itemize
[ "{S.C.lambda_l} for {S.C.asm} sources {texttt s} equipped with these proofs in Coq~:" ^^
  enumerate [ "the associated {SL_p.Coq.Deep.asm} has been obtained successfully,"
          ; "the behavior of the {SL_p.Coq.Deep.compcert} is {emph "not wrong"}." ]
(*" can be successfully transformed to an assembly file with a certified compiler preserving its original semantic (and preserving at least from the {S.C.compcert} big-step semantic). Moreover, the behavior of the initial source is required to be proved {emph "not wrong"}."*)
; "{SL_p.Coq.Deep.lambda_l} will naturally stand for the {SL_p.Coq.Deep.asm} (if {SL_p.C.asm} {in_} {S.C.lambda_l})." ]
}
")

(* ********************************************************* *)
; B.Center ("zz",
            Th.env Label.ex "
This {S.C.asm} code is not a {S.C.lambda_l} program :
<@@{let open English in H_comment (BatList.flatten [ BatList.init 4 (fun _ -> yes) ; [ no ] ])}@
int main(int _) {
  return 0;
}
@>
because the type of the main function (called from {English.outworld}) is not of the form {texttt "unit {rightarrow} int"}. Thus it initially goes wrong by definition.
")

(* ********************************************************* *)
; B.Center ("zz",
            Th.env Label.fact "
The {S.SL.C.asm} is not a {S.C.lambda_l} program because as a simulator, its task is to simulate an arbitrary assembly file given in input.

More clearly~:
<#
Lemma no_initial_state :
  forall s, ~ Csem.initial_state asmC_simlight s.

  Proof.
    intuition. inversion H.
    vm_compute in H1. inversion H1. subst b.
    vm_compute in H2. inversion H2. subst f.
    vm_compute in H3. inversion H3.
  Qed.

Proposition wrong_behavior :
  program_behaves (Csem.semantics asmC_simlight) (Goes_wrong E0).

  Proof.
    apply program_goes_initially_wrong ;
    apply no_initial_state.
  Qed.
#>
(Note that, except ``<!asmC_simlight!>'' which designates {S.SL.C.asm} and has been produced by our pretty-printer, every others free variables are more explained in the original source of CompCert.)
(* see in CompCert /common/Behaviors.v *)
")

(* ********************************************************* *)
; B.Center ("zz",
            let module SL_a = S.SL_gen (struct let sl = "FUN" end) in
            let i_sqcup x = index sqcup (tiny x) in
            Th.env Label.def "
We present here
{itemize
[ "{S.C.infty} being the smallest set satisfying these properties~:" ^^
enumerate [ "${S.C.lambda_l} {subseteq} {S.C.infty}$"
          ; "{forall} {SL_a.C.asm}, {S.P.C.infty}, {newline}
               ({SL_a.C.asm} {i_sqcup "apply"} {S.P.C.infty}) {in_} {S.C.infty}
               {longrightarrow_ }
               {SL_a.C.asm} {in_} {S.C.infty}"
           ]
; "{S.P.Coq.Deep.infty} is introduced as a synonym of {S.P.Coq.Deep.asm} (if {S.P.C.asm} {in_} {S.C.infty})." ]
}
")

(* ********************************************************* *)
; B.Center ("",
"<@@{let open English in H_comment (BatList.flatten [ BatList.init 4 (fun _ -> yes) ; [ maybe ] ])}@
int main() {
  return 0;
}
@>")

(* ********************************************************* *)
; B.Center ("", bibliographystyle "alpha")

(* ********************************************************* *)
; B.Center ("", bibliography "t")

]))
